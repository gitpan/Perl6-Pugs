{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

{-
    Evaluation and reduction engine.

    Home is behind, the world ahead,
    And there are many paths to tread
    Through shadows to the edge of night,
    Until the stars are all alight.
    Then world behind and home ahead,
    We'll wander back to home and bed...
-}

module Pugs.Eval where
import Pugs.Config
import Pugs.Internals
import Prelude hiding ( exp )
import qualified Data.Map as Map

import Pugs.AST
import Pugs.Junc
import Pugs.Bind
import Pugs.Prim
import Pugs.Context
import Pugs.Monads
import Pugs.Pretty
import Pugs.Types

emptyEnv :: (MonadIO m, MonadSTM m) => String -> [STM (Pad -> Pad)] -> m Env
emptyEnv name genPad = do
    uniq <- liftIO newUnique
    liftSTM $ do
        pad  <- sequence genPad
        ref  <- newTVar Map.empty
        syms <- initSyms
        glob <- newTVar (combine (pad ++ syms) $ mkPad [])
        return $ Env
            { envContext = CxtVoid
            , envLexical = mkPad []
            , envLValue  = False
            , envGlobal  = glob
            , envClasses = initTree
            , envEval    = evaluate
            , envCaller  = Nothing
            , envDepth   = 0
            , envID      = uniq
            , envBody    = Val undef
            , envDebug   = Just ref -- Set to "Nothing" to disable debugging
            , envStash   = ""
            , envPos     = MkPos name 1 1 1 1
            }

-- Evaluation ---------------------------------------------------------------

-- debug :: (Pretty a) => String -> String -> a -> Eval ()
debug key fun str a = do
    rv <- asks envDebug
    case rv of
        Nothing -> return ()
        Just ref -> do
            val <- liftSTM $ do
                fm <- readTVar ref
                let val = fun $ Map.findWithDefault "" key fm
                writeTVar ref (Map.insert key val fm)
                return val
            when (length val > 100) $ do
                trace "*** Warning: deep recursion" return ()
            trace ("***" ++ val ++ str ++ pretty a) return ()

evaluateMain :: Exp -> Eval Val
evaluateMain exp = do
    val     <- resetT $ evaluate exp
    endAV   <- evalVar "@*END"
    subs    <- fromVals endAV
    enterContext CxtVoid $ do
        mapM_ evalExp [ Syn "()" [Val sub, Syn "invs" [], Syn "args" []]
                      | sub <- subs
                      ]
    return val

-- |Evaluate an expression. This function mostly just delegates to 'reduce'.
evaluate :: Exp -- ^ The expression to evaluate
         -> Eval Val
evaluate (Val val) = evalVal val
evaluate exp = do
    want <- asks envWant
    debug "indent" (' ':) ("Evl [" ++ want ++ "]:\n") exp
    val <- local (\e -> e{ envBody = exp }) $ reduce exp
    debug "indent" (tail) " Ret: " val
    trapVal val (return val)

findSyms :: Var -> Eval [(String, Val)]
findSyms name = do
    lex  <- asks envLexical
    glob <- askGlobal
    let names = [name, toGlobal name]
    syms <- forM [lex, glob] $ \pad -> do
        forM names $ \name' -> do
            case lookupPad name' pad of
                Just tvar -> do
                    refs  <- liftSTM $ mapM readTVar tvar
                    forM refs $ \ref -> do
                        val <- readRef ref
                        return (name', val)
                Nothing -> return []
    return $ concat (concat syms)

enterEvalContext :: Cxt -> Exp -> Eval Val
enterEvalContext cxt = enterContext cxt . evalExp

-- Reduction ---------------------------------------------------------------

retVal :: Val -> Eval Val
retVal val = evaluate (Val val)  -- casting

addGlobalSym newSym = do
    glob <- asks envGlobal
    liftSTM $ do
        syms <- readTVar glob
        writeTVar glob (newSym syms)

trapVal :: Val -> Eval a -> Eval a
trapVal val action = case val of
    VError str exp  -> retError str exp
    VControl c      -> retControl c
    _               -> action

evalVar :: Ident -> Eval Val
evalVar name = do
    v <- findVar name
    case v of
        Just var -> readRef var
        Nothing  -> retError "Undeclared variable" name

enterLValue = local (\e -> e{ envLValue = True })
enterRValue = local (\e -> e{ envLValue = False })

findVar :: Ident -> Eval (Maybe VRef)
findVar name = do
    rv <- findVarRef name
    case rv of
        Nothing  -> return Nothing
        Just ref -> fmap Just $ liftSTM (readTVar ref)

findVarRef :: Ident -> Eval (Maybe (TVar VRef))
findVarRef name
    | Just (package, name') <- breakOnGlue "::" name
    , Just (sig, "") <- breakOnGlue "CALLER" package = do
        maybeCaller <- asks envCaller
        case maybeCaller of
            Just caller -> local (const caller) $ do
                findVarRef (sig ++ name')
            Nothing -> retError "cannot access CALLER:: in top level" name
    | ('$':'?':_) <- name = do
        rv  <- getMagical name
        case rv of
            Nothing  -> doFindVarRef name
            Just val -> do
                tvar <- liftSTM $ newTVar (MkRef . constScalar $ val)
                return $ Just tvar
    | otherwise = doFindVarRef name
    where
    doFindVarRef name = do
        callCC $ \foundIt -> do
            lexSym <- fmap (findSym name . envLexical) ask
            when (isJust lexSym) $ foundIt lexSym
            glob   <- liftSTM . readTVar . envGlobal =<< ask
            let globSym = findSym name glob
            when (isJust globSym) $ foundIt globSym
            let globSym = findSym (toGlobal name) glob
            when (isJust globSym) $ foundIt globSym
            return Nothing

posSym f = fmap (Just . castV . f) $ asks envPos
constSym = return . Just . castV

-- |Evaluate the 'magical' variable associated with a given name. Returns 
-- Nothing if the name does not match a known magical.
getMagical :: String -- ^ Name of the magical var to evaluate
           -> Eval (Maybe Val)
getMagical "$?FILE"     = posSym posName
getMagical "$?LINE"     = posSym posBeginLine
getMagical "$?COLUMN"   = posSym posBeginColumn
getMagical "$?POSITION" = posSym pretty
getMagical "$?MODULE"   = constSym "main"
getMagical "$?OS"       = constSym $ getConfig "osname"
getMagical _            = return Nothing

-- |Reduce an expression into its value.
reduce :: Exp -- ^ The expression to reduce
       -> Eval Val

-- Reduction for mutables
reduce (Val v@(VRef var)) = do
    lv <- asks envLValue
    if lv then retVal v else do
    rv <- readRef var
    retVal rv

-- Reduction for constants
reduce (Val v) = retVal v

-- Reduction for variables
reduce (Var name) = do
    v <- findVar name
    if isNothing v then retError "Undeclared variable" name else do
    let ref = fromJust v
    if refType ref == (mkType "Thunk") then forceRef ref else do
    val <- callCC $ \esc -> do
        cxt <- asks envContext
        lv  <- asks envLValue
        let typ = typeOfCxt cxt
            isCollectionRef = (refType ref /= mkType "Scalar")
        -- If RValue, read from the reference
        unless lv $ do
            when (isCollectionRef && isItemCxt cxt) $ do
                -- auto-enreference
                esc $ VRef ref
            esc =<< readRef ref
        -- LValue here
        when isCollectionRef $ esc (castV ref)
        val <- readRef ref
        let isAutovivify = and $
                [ not (defined val)
                , isItemCxt cxt
                , (typ ==) `any` [mkType "Array", mkType "Hash"]
                ]
        when isAutovivify $ do
            ref' <- newObject typ
            writeRef ref (VRef ref')
        return $ castV ref
    retVal val

reduce (Stmts this rest) | Noop <- unwrap rest = reduce this
reduce (Stmts this rest) | Noop <- unwrap this = reduce rest

reduce (Stmts this rest) = do
    val <- enterContext cxtVoid $ reduce this
    trapVal val $ case unwrap rest of
        (Syn "env" []) -> do
            env <- ask
            writeVar "$*_" val
            return . VControl $ ControlEnv env
        _ -> reduce rest

reduce (Syn "env" []) = do
    env <- ask
    -- writeVar "$*_" val
    return . VControl $ ControlEnv env

reduce (Pos pos exp) = do
    local (\e -> e{ envPos = pos }) $ do
        evalExp exp

reduce (Pad SMy lex exp) = do
    -- heuristics: if we are repeating ourselves, generate a new TVar.
    lex' <- fmap mkPad $ liftSTM $ forM (padToList lex) $ \(name, tvars) -> do
        tvars' <- forM tvars $ \orig@(fresh, _) -> do
            isFresh <- readTVar fresh
            if isFresh then do { writeTVar fresh False; return orig } else do
            -- regen TVar -- we have re-entered this scope
            ref     <- newObject (typeOfSigil $ head name)
            tvar'   <- newTVar ref
            return (fresh, tvar')
        return (name, tvars')
    local (\e -> e{ envLexical = lex' `unionPads` envLexical e }) $ do
        evalExp exp

reduce (Pad _ lex exp) = do
    local (\e -> e{ envLexical = lex `unionPads` envLexical e }) $ do
        evalExp exp

reduce (Sym scope name exp) = do
    ref <- newObject (typeOfSigil $ head name)
    sym <- case name of
        ('&':_) -> genMultiSym name ref
        _       -> genSym name ref
    case scope of
        SMy -> enterLex [ sym ] $ evalExp exp
        _   -> do { addGlobalSym sym; evalExp exp }

-- Context forcing
reduce (Cxt cxt exp) = do
    val <- enterEvalContext cxt exp
    enterEvalContext cxt (Val val) -- force casting

-- Reduction for no-operations
reduce Noop = retEmpty

-- Reduction for syntactic constructs
reduce exp@(Syn name exps) = case name of
    "block" -> do
        let [body] = exps
        enterBlock $ reduce body
    "sub" -> do
        let [exp] = exps
        (VCode sub) <- enterEvalContext (cxtItem "Code") exp
        lex <- asks envLexical
        retVal $ VCode sub{ subPad = lex }
    "if" -> doCond id 
    "unless" -> doCond not
    "for" -> do
        let [list, body] = exps
        av    <- enterLValue $ enterEvalContext cxtSlurpyAny list
        vsub  <- enterRValue $ enterEvalContext (cxtItem "Code") body
        -- XXX this is wrong -- should use Array.next
        sz    <- join $ doArray av array_fetchSize
        fetch <- doArray av array_fetchElem
        elms  <- mapM fetch [0..sz-1]
        VCode sub <- fromVal vsub
        -- XXX: need clarification -- this makes
        --      for @x { ... } into for @x -> $_ {...}
        let arity = max 1 $ length (subParams sub)
            runBody [] _ = retVal undef
            runBody vs sub' = do
                let (these, rest) = arity `splitAt` vs
                genSymCC "&next" $ \symNext -> do
                genSymPrim "&redo" (const $ runBody vs sub') $ \symRedo -> do
                    apply sub'{ subPad = symRedo . symNext $ subPad sub' } [] $
                        map (Val . VRef . MkRef) these
                runBody rest sub'
        genSymCC "&last" $ \symLast -> do
            let munge sub | subParams sub == [defaultArrayParam] =
                    munge sub{ subParams = [defaultScalarParam] }
                munge sub = sub{ subPad = symLast $ subPad sub }
            runBody elms $ munge sub
    "loop" -> do
        let [pre, cond, post, body] = case exps of { [_] -> exps'; _ -> exps }
            exps' = [emptyExp, Val (VBool True), emptyExp] ++ exps
        evalExp pre
        enterLoop . fix $ \runBody -> do
            genSymPrim "&redo" (const $ runBody) $ \symRedo -> do
            valBody <- enterLex [symRedo] $ evalExp body
            valPost <- evalExp post
            vBool   <- enterEvalContext (cxtItem "Bool") cond
            vb      <- fromVal vBool
            trapVal valBody $ do
                trapVal valPost $ do
                    if vb
                        then runBody
                        else retVal valBody
    "given" -> do
        let [topic, body] = exps
        vtopic <- fromVal =<< enterLValue (enterEvalContext cxtItemAny topic)
        enterGiven vtopic $ enterEvalContext (cxtItem "Code") body
    "when" -> do
        let [match, body] = exps
        break  <- evalVar "&?BLOCK_EXIT"
        vbreak <- fromVal break
        match  <- reduce match
        topic  <- evalVar "$_"
        result <- op2Match topic match
        rb     <- fromVal result
        if rb
            then enterWhen (subBody vbreak) $ apply vbreak [body] []
            else retVal undef
    "default" -> do
        let [body] = exps
        break  <- evalVar "&?BLOCK_EXIT"
        vbreak <- fromVal break
        enterWhen (subBody vbreak) $ apply vbreak [body] []
    "while" -> doWhileUntil id
    "until" -> doWhileUntil not
    "=" -> do
        let [lhs, rhs] = exps
        refVal  <- enterLValue $ evalExp lhs
        ref     <- fromVal refVal
        cls     <- asks envClasses
        let typ = refType ref
            cxt | isaType cls "List" typ = cxtSlurpyAny
                | otherwise = cxtItem $ takeWhile (/= ':') . show $ refType ref
        val <- enterRValue $ enterEvalContext cxt rhs
        writeRef ref val
        lv  <- asks envLValue
        retVal $ if lv then refVal else val
    "::=" -> reduce (Syn ":=" exps)
    ":=" | [Syn "," vars, Syn "," vexps] <- unwrap exps -> do
        when (length vars > length vexps) $ do
            fail $ "Wrong number of binding parameters: "
                ++ (show $ length vexps) ++ " actual, "
                ++ (show $ length vars) ++ " expected"
        -- env' <- cloneEnv env -- FULL THUNKING
        names <- forM vars $ \var -> case unwrap var of
            Var name -> return name
            _        -> retError "Cannot bind this as lhs" var
        bindings <- forM (names `zip` vexps) $ \(name, vexp) -> do
            {- FULL THUNKING
            let ref = thunkRef . MkThunk $ do
                    local (const env'{ envLValue = True }) $ do
                        enterEvalContext (cxtOfSigil $ head name) vexp
            -}
            val  <- enterLValue $ enterEvalContext (cxtOfSigil $ head name) vexp
            ref  <- fromVal val
            rv   <- findVarRef name
            case rv of
                Just ioRef -> return (ioRef, ref)
                Nothing -> do
                    retError "Undeclared variable" name
        forM_ bindings $ \(ioRef, ref) -> do
            liftSTM $ writeTVar ioRef ref
        return $ case map (VRef . snd) bindings of
            [v] -> v
            vs  -> VList vs
    ":=" -> do
        let [var, vexp] = exps
            expand e | e'@(Syn "," _) <- unwrap e = e'
            expand e = Syn "," [e]
        reduce (Syn ":=" [expand var, expand vexp])
    "=>" -> do
        let [keyExp, valExp] = exps
        key     <- enterEvalContext cxtItemAny keyExp
        val     <- evalExp valExp
        retVal $ castV (key, val)
    "*" | [Syn syn [exp]] <- unwrap exps --  * cancels out [] and {}
        , syn == "\\{}" || syn == "\\[]"
        -> enterEvalContext cxtSlurpyAny exp
    "*" -> do -- first stab at an implementation
        let [exp] = exps
        val     <- enterRValue $ enterEvalContext cxtSlurpyAny exp
        vals    <- fromVals val
        retVal $ VList $ concat vals
    "," -> do
        vals <- mapM evalExp exps
        -- retVal . VList . concat $ map vCast vals
        -- in slurpy context, flatten each arguments
        ifListContext
            (retVal . VList . concat $ map vCast vals)
            (retVal $ VList vals)
    "val" -> do
        let [exp] = exps
        enterRValue $ evalExp exp
    "\\{}" -> do
        let [exp] = exps
        v   <- enterRValue $ enterEvalContext cxtItemAny exp
        hv  <- newObject (MkType "Hash")
        writeRef hv v
        retVal $ VRef hv
    "\\[]" -> do
        let [exp] = exps
        v   <- enterRValue $ enterEvalContext cxtSlurpyAny exp
        av  <- newObject (MkType "Array")
        writeRef av v
        retVal $ VRef av
    -- XXX evil hack for infinite slices
    "[]" | [lhs, App "&postfix:..." invs args] <- unwrap exps
         , [idx] <- invs ++ args
--       , not (envLValue env)
         -> reduce (Syn "[...]" [lhs, idx])
    "[]" | [lhs, App "&infix:.." invs args] <- unwrap exps
         , [idx, Val (VNum n)] <- invs ++ args
         , n == 1/0
--       , not (envLValue env)
         -> reduce (Syn "[...]" [lhs, idx])
    "[]" -> do
        let [listExp, indexExp] = exps
        idxCxt  <- cxtOfExp indexExp 
        {- if envLValue env
            then cxtOfExp indexExp else return (envContext env)
        -}
        idxVal  <- enterRValue $ enterEvalContext idxCxt indexExp
        varVal  <- enterLValue $ enterEvalContext (cxtItem "Array") listExp
        lv      <- asks envLValue
        doFetch (mkFetch $ doArray varVal array_fetchElem)
                (mkFetch $ doArray varVal array_fetchVal)
                (fromVal idxVal) lv (not (isSlurpyCxt idxCxt))
    "[...]" -> do
        let [listExp, indexExp] = exps
        idxVal  <- enterRValue $ enterEvalContext (cxtItem "Int") indexExp
        idx     <- fromVal idxVal
        listVal <- enterLValue $ enterEvalContext (cxtItem "Array") listExp
        list    <- fromVal listVal
        elms    <- mapM fromVal list -- flatten
        retVal $ VList (drop idx $ concat elms)
    "{}" -> do
        let [listExp, indexExp] = exps
        idxCxt  <- cxtOfExp indexExp 
        {- if envLValue env
            then cxtOfExp indexExp else return (envContext env)
        -}
        idxVal  <- enterRValue $ enterEvalContext idxCxt indexExp
        varVal  <- enterLValue $ enterEvalContext (cxtItem "Hash") listExp
        lv      <- asks envLValue
        doFetch (mkFetch $ doHash varVal hash_fetchElem)
                (mkFetch $ doHash varVal hash_fetchVal)
                (fromVal idxVal) lv (not (isSlurpyCxt idxCxt))
    "()" -> do
        let [subExp, Syn "invs" invs, Syn "args" args] = exps
        vsub <- enterEvalContext (cxtItem "Code") subExp
        (`juncApply` [ApplyArg "" vsub False]) $ \[arg] -> do
            sub  <- fromVal $ argValue arg
            apply sub invs args
    "try" -> do
        val <- resetT $ evalExp (head exps)
        retEvalResult False val
    "gather" -> do
        val     <- enterEvalContext cxtSlurpyAny exp
        -- ignore val
        retVal val
    "rx" -> do
        let [exp, adverbs] = exps
        hv      <- fromVal =<< evalExp adverbs
        val     <- enterEvalContext (cxtItem "Str") exp
        str     <- fromVal val
        p5      <- fromAdverb hv ["P5", "Perl5", "perl5"]
        p5flags <- fromAdverb hv ["P5", "Perl5", "perl5"]
        flag_g  <- fromAdverb hv ["g", "global"]
        flag_i  <- fromAdverb hv ["i", "ignorecase"]
        when (not p5) $ do
            fail "Perl 6 rules is not implemented yet, use :P5"
        retVal $ VRule $ MkRule
            { rxRegex  = mkRegexWithPCRE (encodeUTF8 str) $
                [ pcreUtf8
                , ('i' `elem` p5flags || flag_i) `implies` pcreCaseless
                , ('m' `elem` p5flags) `implies` pcreMultiline
                , ('s' `elem` p5flags) `implies` pcreDotall
                , ('x' `elem` p5flags) `implies` pcreExtended
                ] 
            , rxGlobal = ('g' `elem` p5flags || flag_g)
            }
        where
        implies True  = id
        implies False = const 0
        fromAdverb _ [] = fromVal undef
        fromAdverb hv (k:ks) = case lookup k hv of
            Just v  -> fromVal v
            Nothing -> fromAdverb hv ks
    "subst" -> do
        let [exp, subst, adverbs] = exps
        (VRule rx)  <- reduce (Syn "rx" [exp, adverbs])
        retVal $ VSubst (rx, subst)
    "is" -> do
        retEmpty
    "module" -> do
        let [exp] = exps
        val <- evalExp exp
        writeVar "$?MODULE" val
        retEmpty
    "inline" -> do
        let [langExp, _] = exps
        langVal <- evalExp langExp
        lang    <- fromVal langVal
        when (lang /= "Haskell") $
            retError "Inline: Unknown language" langVal
        modVal  <- readVar "$?MODULE"
        mod     <- fromVal modVal
#ifndef HADDOCK
        let file = (`concatMap` mod) $ \v -> case v of
            { '-' -> "__"; _ | isAlphaNum v -> [v] ; _ -> "_" }
#endif
        op1 "require_haskell" (VStr $ file ++ ".o")
        retEmpty
    syn | last syn == '=' -> do
        let [lhs, exp] = exps
            op = "&infix:" ++ init syn
        evalExp $ Syn "=" [lhs, App op [lhs, exp] []]
    _ -> retError "Unknown syntactic construct" exp
    where
    doCond f = do
        let [cond, bodyIf, bodyElse] = exps
        vbool     <- enterEvalContext (cxtItem "Bool") cond
        vb        <- fromVal vbool
        if (f vb)
            then reduce bodyIf
            else reduce bodyElse
    -- XXX This treatment of while/until loops probably needs work
    doWhileUntil f = do
        let [cond, body] = exps
        enterLoop . fix $ \runLoop -> do
            vbool <- enterEvalContext (cxtItem "Bool") cond
            vb    <- fromVal vbool
            case f vb of
                True -> fix $ \runBody -> do
                    genSymPrim "&redo" (const $ runBody) $ \symRedo -> do
                    rv <- enterLex [symRedo] $ reduce body
                    case rv of
                        VError _ _  -> retVal rv
                        _           -> runLoop
                _ -> retVal vbool

-- XXX absolutely evil bloody hack for context hinters
reduce (App "&hash" invs args) =
    enterEvalContext cxtItemAny $ Syn "\\{}" [Syn "," $ invs ++ args]

reduce (App "&list" invs args) =
    enterEvalContext cxtSlurpyAny $ case invs ++ args of
        []    -> Val (VList [])
        [exp] -> exp
        exps  -> Syn "," exps

reduce (App "&scalar" invs args)
    | [exp] <- invs ++ args = enterEvalContext cxtItemAny exp
    | otherwise = enterEvalContext cxtItemAny $ Syn "," (invs ++ args)

-- XXX absolutely evil bloody hack for "zip"
reduce (App "&zip" invs args) = do
    vals <- mapM (enterRValue . enterEvalContext (cxtItem "Array")) (invs ++ args)
    val  <- op0 "Y" vals
    retVal val

-- XXX absolutely evil bloody hack for "goto"
reduce (App "&goto" (subExp:invs) args) = do
    vsub <- enterEvalContext (cxtItem "Code") subExp
    sub <- fromVal vsub
    local callerEnv $ do
        val <- apply sub invs args
        shiftT $ const (retVal val)
    where
    callerEnv env = let caller = maybe env id (envCaller env) in
        env{ envCaller = envCaller caller
           , envContext = envContext caller
           , envLValue = envLValue caller
           }

-- XXX absolutely evil bloody hack for "assuming"
reduce (App "&assuming" (subExp:invs) args) = do
    vsub <- enterEvalContext (cxtItem "Code") subExp
    sub <- fromVal vsub
    case bindSomeParams sub invs args of
        Left errMsg      -> fail errMsg
        Right curriedSub -> retVal $ castV $ curriedSub

reduce (App "&infix:=>" [keyExp, valExp] []) = do
    key <- enterEvalContext cxtItemAny keyExp
    val <- enterEvalContext cxtItemAny valExp
    retVal $ castV (key, val)

reduce (App name invs args) = do
    sub <- findSub name invs args
    case sub of
        Just sub    -> applySub sub invs args
        Nothing     -> retError "No compatible subroutine found" name
    where
    applySub sub invs args
        -- list-associativity
        | MkCode{ subAssoc = "list" }      <- sub
        , (App name' invs' []):rest  <- invs
        , name == name'
        = applySub sub (invs' ++ rest)  []
        -- fix subParams to agree with number of actual arguments
        | MkCode{ subAssoc = "list", subParams = (p:_) }   <- sub
        , null args
        = apply sub{ subParams = (length invs) `replicate` p } invs []
        -- chain-associativity
        | MkCode{ subAssoc = "chain" }   <- sub
        , (App _ _ []):_              <- invs
        , null args
        = mungeChainSub sub invs
        | MkCode{ subAssoc = "chain", subParams = (p:_) }   <- sub
        = apply sub{ subParams = (length invs) `replicate` p } invs []
        -- normal application
        | otherwise
        = apply sub invs args
    mungeChainSub sub invs = do
        let MkCode{ subAssoc = "chain", subParams = (p:_) } = sub
            (App name' invs' args'):rest = invs
        theSub   <- findSub name' invs' args'
        case theSub of
            Just sub'    -> applyChainSub sub invs sub' invs' args' rest
            Nothing      -> apply sub{ subParams = (length invs) `replicate` p } invs [] -- XXX Wrong
    applyChainSub sub invs sub' invs' args' rest
        | MkCode{ subAssoc = "chain", subBody = fun, subParams = prm }   <- sub
        , MkCode{ subAssoc = "chain", subBody = fun', subParams = prm' } <- sub'
        , null args'
        = applySub sub{ subParams = prm ++ tail prm', subBody = Prim $ chainFun prm' fun' prm fun } (invs' ++ rest) []
        | MkCode{ subAssoc = "chain", subParams = (p:_) }   <- sub
        = apply sub{ subParams = (length invs) `replicate` p } invs [] -- XXX Wrong
        | otherwise
        = internalError "applyChainsub did not match a chain subroutine"

reduce (Parens exp) = reduce exp
reduce exp = retError "Invalid expression" exp

-- |Return the context that an expression bestows upon a hash or array
-- subscript. See 'reduce' for \{\} and \[\]. (Is this correct?)
cxtOfExp :: Exp -- ^ Expression to find the context of
         -> Eval Cxt
cxtOfExp (Pos _ exp)            = cxtOfExp exp
cxtOfExp (Parens exp)           = cxtOfExp exp
cxtOfExp (Cxt cxt _)            = return cxt
cxtOfExp (Syn "," _)            = return cxtSlurpyAny
cxtOfExp (Syn "[]" [_, exp])    = cxtOfExp exp
cxtOfExp (Syn "{}" [_, exp])    = cxtOfExp exp
cxtOfExp (Val (VList _))        = return cxtSlurpyAny
cxtOfExp (Val (VRef ref))       = do
    cls <- asks envClasses
    let typ = refType ref
    return $ if isaType cls "List" typ
        then cxtSlurpyAny
        else CxtItem typ
cxtOfExp (Val _)                = return cxtItemAny
cxtOfExp (Var (c:_))            = return $ cxtOfSigil c
cxtOfExp (App "&list" _ _)      = return cxtSlurpyAny
cxtOfExp (App "&scalar" _ _)    = return cxtSlurpyAny
cxtOfExp (App name invs args)   = do
    -- inspect the return type of the function here
    env <- ask
    sub <- findSub name invs args
    return $ case sub of
        Just sub
            | isaType (envClasses env) "Scalar" (subReturns sub)
            -> CxtItem (subReturns sub)
        _ -> cxtSlurpyAny
cxtOfExp _                      = return cxtSlurpyAny


findSub name invs args = do
    subSyms <- findSyms name
    lens    <- mapM argSlurpLen (unwrap $ invs ++ args)
    doFindSub (sum lens) subSyms
    where
    argSlurpLen (Val listMVal) = do
        listVal  <- fromVal listMVal
        return $ length (vCast listVal :: [Val])
    argSlurpLen (Var name) = do
        listMVal <- evalVar name
        listVal  <- fromVal listMVal
        return $ length (vCast listVal :: [Val])
    argSlurpLen (Syn "," list) =  return $ length list
    argSlurpLen _ = return 1 -- XXX
    doFindSub slurpLen subSyms = do
        subs' <- subs slurpLen subSyms
        -- let foo (x, sub) = show x ++ show (map paramContext $ subParams sub)
        -- trace (unlines $ map foo $ sort subs') return ()
        return $ case sort subs' of
            ((_, sub):_)    -> Just sub
            _               -> Nothing
    subs slurpLen subSyms = (liftM catMaybes) $ (`mapM` subSyms) $ \(n, val) -> do
        sub@(MkCode{ subType = subT, subReturns = ret, subParams = prms }) <- fromVal val
        let isGlobal = '*' `elem` n
        let fun = arityMatch sub (length (invs ++ args)) slurpLen
        if isNothing fun then return Nothing else do
        -- if deltaFromCxt ret == 0 then return Nothing else do
        let invocants = filter isInvocant prms
        let prms' = if null invocants then prms else invocants
        deltaCxt    <- deltaFromCxt ret
        deltaArgs   <- mapM (deltaFromScalar . typeOfCxt . paramContext) prms'
        let distance = (deltaCxt : deltaArgs)
        let bound = either (const False) (const True) $ bindParams sub invs args
        return $ Just
            ( (isGlobal, subT, isMulti sub, bound, distance)
            , fromJust fun
            )
    deltaFromCxt x  = do
        cls <- asks envClasses
        cxt <- asks envContext
        return $ deltaType cls (typeOfCxt cxt) x
    deltaFromScalar x = do
        cls <- asks envClasses
        return $ deltaType cls x (mkType "Scalar")


-- OK... Now let's implement the hideously clever autothreading algorithm.
-- First pass - thread thru all() and none()
-- Second pass - thread thru any() and one()

chainFun :: Params -> Exp -> Params -> Exp -> [Val] -> Eval Val
chainFun p1 f1 p2 f2 (v1:v2:vs) = do
    val <- applyExp (chainArgs p1 [v1, v2]) f1
    case val of
        VBool False -> return val
        _           -> applyExp (chainArgs p2 (v2:vs)) f2
    where
    chainArgs prms vals = map chainArg (prms `zip` vals)
    chainArg (p, v) = ApplyArg (paramName p) v False
chainFun _ _ _ _ _ = internalError "chainFun: Not enough parameters in Val list"

applyExp :: [ApplyArg] -> Exp -> Eval Val
applyExp bound (Prim f) =
    f [ argValue arg | arg <- bound, (argName arg !! 1) /= '_' ]
applyExp bound body = do
    pad <- formal
    enterLex pad $ evalExp body
    where
    formal = mapM argNameValue $ filter (not . null . argName) bound
    argNameValue (ApplyArg name val _) = genSym name (vCast val)

apply :: VCode -> [Exp] -> [Exp] -> Eval Val
apply sub invs args = do
    env <- ask
    doApply env sub invs args

-- XXX - faking application of lexical contexts
-- XXX - what about defaulting that depends on a junction?
doApply :: Env -> VCode -> [Exp] -> [Exp] -> Eval Val
doApply Env{ envClasses = cls } sub@MkCode{ subBody = fun, subType = typ } invs args =
    case bindParams sub invs args of
        Left errMsg -> fail errMsg
        Right sub   -> do
            forM_ (subSlurpLimit sub) $ \limit@(n, _) -> do
                extra <- checkSlurpyLimit limit
                when (not $ null extra) $ do
                    fail $
                        "Too many slurpy arguments for " ++ subName sub ++ ": "
                        ++ show ((genericLength (take 1000 extra)) + n) ++ " actual, "
                        ++ show n ++ " expected"
            enterScope $ do
                (syms, bound) <- doBind [] (subBindings sub)
                -- trace (show bound) $ return ()
                val <- local fixEnv $ enterLex syms $ do
                    (`juncApply` bound) $ \realBound -> do
                        enterSub sub $ do
                            applyExp realBound fun
                retVal val
    where
    enterScope :: Eval Val -> Eval Val
    enterScope
        | typ >= SubBlock = id
        | otherwise       = resetT
    fixEnv env
        | typ >= SubBlock = env
        | otherwise       = env{ envCaller = Just env }
    doBind :: [Pad -> Pad] -> [(Param, Exp)] -> Eval ([Pad -> Pad], [ApplyArg])
    doBind syms [] = return (syms, [])
    doBind syms ((prm, exp):rest) = do
        -- trace ("<== " ++ (show (prm, exp))) $ return ()
        let name = paramName prm
            cxt = cxtOfSigil $ head name
        (val, coll) <- enterContext cxt $ case exp of
            Parens exp  -> local fixEnv $ enterLex syms $ expToVal prm exp
            _           -> expToVal prm exp
        -- trace ("==> " ++ (show val)) $ return ()
        boundRef <- fromVal val
        newSym   <- genSym name boundRef
        (syms', restArgs) <- doBind (newSym:syms) rest
        return (syms', ApplyArg name val coll:restArgs)
    expToVal MkParam{ isLazy = thunk, isLValue = lv, paramContext = cxt, paramName = name, isWritable = rw } exp = do
        env <- ask -- freeze environment at this point for thunks
        let eval = local (const env{ envLValue = lv }) $ do
            enterEvalContext (cxtOfSigil $ head name) exp
        val <- if thunk then return (VRef . thunkRef $ MkThunk eval) else do
            v   <- eval
            typ <- evalValType v
            if isaType cls "Junction" typ then return v else do
            case (lv, rw) of
                (True, True)    -> return v
                (True, False)   -> return (VRef $ scalarRef v) 
                (False, False)  -> return v -- XXX reduce to val?
                (False, True)   -> do
                    -- make a copy
                    ref <- newObject (typeOfSigil $ head name)
                    writeRef ref v
                    return (VRef ref)
        return (val, (isSlurpyCxt cxt || isCollapsed (typeOfCxt cxt)))
    checkSlurpyLimit (n, exp) = do
        listVal <- enterLValue $ enterEvalContext (cxtItem "Array") exp
        list    <- fromVal listVal
        elms    <- mapM fromVal list -- flatten
        return $ genericDrop n (concat elms :: [Val])
    isCollapsed typ
        | isaType cls "Bool" typ        = True
        | isaType cls "Junction" typ    = True
        | otherwise                     = False

toGlobal name
    | (sigil, identifier) <- break (\x -> isAlpha x || x == '_') name
    , last sigil /= '*'
    = sigil ++ ('*':identifier)
    | otherwise = name


arityMatch sub@MkCode{ subAssoc = assoc, subParams = prms } argLen argSlurpLen
    | assoc == "list" || assoc == "chain"
    = Just sub
    | isNothing $ find (not . isSlurpy) prms -- XXX - what about empty ones?
    , assoc == "pre"
    , slurpLen <- length $ filter (\p -> isSlurpy p && head (paramName p) == '$') prms
    , hasArray <- isJust $ find (\p -> isSlurpy p && head (paramName p) == '@') prms
    , if hasArray then slurpLen <= argSlurpLen else slurpLen == argSlurpLen
    = Just sub
    | reqLen <- length $ filter (\p -> not (isOptional p || isSlurpy p)) prms
    , optLen <- length $ filter (\p -> isOptional p) prms
    , hasArray <- isJust $ find (\p -> isSlurpy p && head (paramName p) == '@') prms
    , argLen >= reqLen && (hasArray || argLen <= (reqLen + optLen))
    = Just sub
    | otherwise
    = Nothing

doFetch :: (Val -> Eval (IVar VScalar))
            -> (Val -> Eval Val)
            -> (forall v. (Value v) => Eval v)
            -> Bool -> Bool
            -> Eval Val
doFetch fetchElem fetchVal fetchIdx isLV isSV = case (isLV, isSV) of
    (True, True) -> do
        -- LValue, Scalar context
        idx <- fetchIdx
        elm <- fetchElem idx
        retIVar elm
    (True, False) -> do
        -- LValue, List context
        idxList <- fetchIdx
        elms    <- mapM fetchElem idxList
        retIVar $ IArray elms
    (False, True) -> do
        -- RValue, Scalar context
        idx <- fetchIdx
        fetchVal idx
    (False, False) -> do
        -- RValue, List context
        idxList <- fetchIdx
        fmap VList $ mapM fetchVal idxList

mkFetch :: (Value n) => Eval (n -> Eval t) -> Val -> Eval t
mkFetch f v = do
    f' <- f
    v' <- fromVal v
    f' v'
