
module Cast where
import Internals
import AST
import Lexer

instance Value VStr where
    castV = VStr
    fromVal (VHash (MkHash h)) = do
        ls <- mapM strPair $ fmToList h
        return $ unlines ls
        where
        strPair (k, v) = do
            k' <- fromMVal k
            v' <- fromMVal v
            return $ k' ++ "\t" ++ v'
    fromVal v = fromVal' v
    vCast VUndef        = ""
    vCast (VStr s)      = s
    vCast (VBool b)     = if b then "1" else ""
    vCast (VInt i)      = show i
    vCast (VRat r)      = showNum $ (realToFrac r :: Double)
    vCast (VNum n)      = showNum n
    vCast (VList l)     = unwords $ map vCast l
    vCast (VRef v)      = vCast v
    -- vCast (MVal v)      = vCast $ castV v
    vCast (VPair (k, v))= vCast k ++ "\t" ++ vCast v ++ "\n"
    vCast (VArray (MkArray l))   = unwords $ map vCast l
    vCast (VHash (MkHash h))     = unlines $ map (\(k, v) -> (vCast k ++ "\t" ++ vCast v)) $ fmToList h
    vCast (VSub s)      = "<" ++ show (subType s) ++ "(" ++ subName s ++ ")>"
    vCast (VJunc j)     = show j
    vCast x             = error $ "cannot cast as Str: " ++ (show x)

instance Show VJunc where
    show (Junc jtype _ set) =
       	(show jtype) ++ "(" ++
	    (foldl (\x y ->
		if x == "" then (vCast :: Val -> VStr) y
		else x ++ "," ++ (vCast :: Val -> VStr) y)
	    "" $ setToList set) ++ ")"

instance Value [Word8] where doCast = map (toEnum . ord) . vCast
