{-# OPTIONS_GHC -fglasgow-exts #-}
module Pugs.Parser.Util where

import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Lexer
import Pugs.Rule

import Pugs.Parser.Types
import qualified Data.Map as Map

fixities :: [String]
fixities = ["prefix_circumfix_meta_operator:","infix_circumfix_meta_operator:","prefix_postfix_meta_operator:","postfix_prefix_meta_operator:","infix_postfix_meta_operator:","statement_modifier:","statement_control:","scope_declarator:","trait_auxiliary:","trait_verb:","regex_mod_external:","regex_mod_internal:","regex_assertion:","regex_backslash:","regex_metachar:","postcircumfix:","circumfix:","postfix:","infix:","prefix:","quote:","term:"]

-- around a block body we save the package and the current lexical pad
-- at the start, so that they can be restored after parsing the body
localEnv :: RuleParser Exp -> RuleParser Exp
localEnv m = do
    state   <- get
    let env = ruleEnv state
    put state
        { ruleBlockPads = Map.empty
        , ruleEnv = env { envOuter = Just env }
        }
    rv      <- m
    state'  <- get
    put state
        { ruleEnv = (ruleEnv state')
            { envPackage = envPackage env
            , envLexical = envLexical env
            , envOuter   = envOuter env
            }
        }
    -- Hoist all pad-declared entries into this block
    -- XXX - Handle "state" and "constant" here.
    return $ Map.foldWithKey Pad rv (ruleBlockPads state')

ruleParamList :: ParensOption -> RuleParser a -> RuleParser (Maybe [[a]])
ruleParamList wantParens parse = rule "parameter list" $ do
    (formal, hasParens) <- f $
        (((try parse) `sepEndBy` ruleComma) `sepEndBy` invColon)
    case formal of
        [[]]   -> return $ if hasParens then Just [[], []] else Nothing
        [args] -> return $ Just [[], args]
        [_,_]  -> return $ Just formal
        _      -> fail "Only one invocant list allowed"
    where
    f = case wantParens of
        ParensOptional  -> maybeParensBool
        ParensMandatory -> \x -> do rv <- parens x; return (rv, True)
    invColon = do
        string ":"
        -- Compare:
        --   sub foo (: $a)   # vs.
        --   sub foo (:$a)
        lookAhead $ (many1 space <|> string ")")
        whiteSpace
        return ":"
        
maybeParensBool :: RuleParser a -> RuleParser (a, Bool)
maybeParensBool p = choice
    [ do rv <- parens p; return (rv, True)
    , do rv <- p; return (rv, False)
    ]


isOperatorName :: String -> Bool
isOperatorName ('&':name) = any hasOperatorPrefix [name, tail name]
    where
    hasOperatorPrefix :: String -> Bool
    hasOperatorPrefix name = any (`isPrefixOf` name) fixities
isOperatorName _ = False


{-| Wraps a call to @&Pugs::Internals::check_for_io_leak@ around the input
    expression. @&Pugs::Internals::check_for_io_leak@ should @die()@ if the
    expression returned an IO handle. -}
-- Please remember to edit Prelude.pm, too, if you rename the name of the
-- checker function.
checkForIOLeak :: Exp -> Exp
checkForIOLeak exp =
    App (Var "&Pugs::Internals::check_for_io_leak") Nothing
        [ Val $ VCode mkSub { subBody = exp } ]
    
defaultParamFor :: SubType -> [Param]
defaultParamFor SubBlock    = [defaultScalarParam]
defaultParamFor SubPointy   = []
defaultParamFor _           = [defaultArrayParam]

doExtract :: SubType -> Maybe [Param] -> Exp -> (Exp, [String], [Param])
doExtract SubBlock formal body = (fun, names', params)
    where
    (fun, names) = extractPlaceholderVars body []
    names' | isJust formal
           = filter (/= "$_") names
           | otherwise
           = names
    params = map nameToParam (sort names') ++ (maybe [] id formal)
doExtract SubPointy formal body = (body, [], maybe [] id formal)
doExtract SubMethod formal body = (body, [], maybe [] id formal)
doExtract _ formal body = (body, names', params)
    where
    (_, names) = extractPlaceholderVars body []
    names' | isJust formal
           = filter (/= "$_") names
           | otherwise
           = filter (== "$_") names
    params = map nameToParam (sort names') ++ (maybe [] id formal)

nameToParam :: String -> Param
nameToParam name = MkParam
    { isInvocant    = False
    , isOptional    = False
    , isNamed       = False
    , isLValue      = True
    , isWritable    = (name == "$_")
    , isLazy        = False
    , paramName     = name
    , paramContext  = case name of
        -- "$_" -> CxtSlurpy $ typeOfSigil (head name)
        _    -> CxtItem   $ typeOfSigil (head name)
    , paramDefault  = Noop
    }

paramsFor :: SubType -> Maybe [Param] -> [Param] -> [Param]
paramsFor SubMethod formal params 
    | isNothing (find (("%_" ==) . paramName) params)
    = paramsFor SubRoutine formal params ++ [defaultHashParam]
paramsFor styp Nothing []       = defaultParamFor styp
paramsFor _ _ params            = params

processFormals :: Monad m => [[Exp]] -> m (Maybe Exp, [Exp])
processFormals formal = case formal of
    []      -> return (Nothing, [])
    [args]  -> return (Nothing, unwind args)
    [invs,args] | [inv] <- unwind invs -> return (Just inv, unwind args)
    _                   -> fail "Only one invocant allowed"
    where
    unwind :: [Exp] -> [Exp]
    unwind [] = []
    unwind ((Syn "," list):xs) = unwind list ++ unwind xs
    unwind x  = x

-- | A Param representing the default (unnamed) invocant of a method on the given type.
selfParam :: String -> Param
selfParam typ = MkParam
    { isInvocant    = True
    , isOptional    = False
    , isNamed       = False
    , isLValue      = True
    , isWritable    = True
    , isLazy        = False
    , paramName     = "$?SELF"
    , paramContext  = CxtItem (mkType typ)
    , paramDefault  = Noop
    }

extractHash :: Exp -> Maybe Exp
extractHash exp = extractHash' (possiblyUnwrap exp)
    where
    possiblyUnwrap (Syn "block" [exp]) = exp
    possiblyUnwrap (App (Val (VCode (MkCode { subType = SubBlock, subBody = fun }))) Nothing []) = fun
    possiblyUnwrap x = x
    
    isHashOrPair (Ann _ exp) = isHashOrPair exp
    isHashOrPair (App (Var "&pair") _ _) = True
    isHashOrPair (App (Var "&infix:=>") _ _) = True
    isHashOrPair (Var ('%':_)) = True
    isHashOrPair (Syn "%{}" _) = True
    isHashOrPair _ = False
    
    extractHash' (Ann _ exp) = extractHash' exp
    extractHash' exp                      | isHashOrPair exp    = Just exp
    extractHash' exp@(Syn "," (subexp:_)) | isHashOrPair subexp = Just exp
    extractHash' exp@Noop = Just exp
    extractHash' _ = Nothing
