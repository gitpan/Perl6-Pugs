{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Internals (
    Arbitrary(..),
    gen1, gen2, assert,
    newSTRef, readSTRef, writeSTRef, modifySTRef, runST, ST, STRef,
    test, oneof, quickCheck, verboseCheck, Id, newId,
    TVar, STM, newTVar, readTVar, writeTVar,
    fmapM, Map, atomically, Dynamic, Typeable, toDyn,
) where
import Control.Monad.ST
import Data.STRef
import Test.QuickCheck
import Control.Exception
import Control.Concurrent.STM
import System.IO.Unsafe (unsafePerformIO)
import Data.FunctorM
import Data.Map (Map)
import Data.Dynamic
import Data.Typeable

gen1 :: (Arbitrary a) => (a -> b) -> Gen b
gen1 = (`fmap` arbitrary)

gen2 :: (Arbitrary a, Arbitrary b) => (a -> b -> c) -> Gen c
gen2 f = do
    x <- arbitrary
    y <- arbitrary
    return $ f x y

{-|
'Id' is an unique integer, with infinite supply.
-} 
newtype Id = MkId Int
    deriving (Eq, Ord, Show, Num, Arbitrary)

{-# NOINLINE idSource #-}
idSource :: TMVar Int
idSource = unsafePerformIO $ atomically (newTMVar 0)

newId :: STM Id
newId = do
   val <- takeTMVar idSource
   let next = val+1
   putTMVar idSource next
   return (MkId next)

instance (Typeable a, Typeable b) => Show (a -> b) where
    show _ = "(" ++ typA ++ " -> " ++ typB ++ ")"
        where
        typA = show $ typeOf (undefined :: a)
        typB = show $ typeOf (undefined :: b)
instance (Typeable a, Typeable b) => Eq (a -> b) where
    x == y = show x == show y
instance (Typeable a, Typeable b) => Ord (a -> b) where
    compare x y = compare (show x) (show y)