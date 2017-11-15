{-# LANGUAGE ExistentialQuantification #-}

module Lib
    ( ThreadSafeList(..)
    , ListType(..)
    , allTypes
    , newEmptyList
    , fromList
    , add
    , remove
    , toPureList
    , contains
    ) where

import Data.Foldable

import qualified ThreadSafeList as TSL
import qualified CoarseGrained as CG
import qualified FineGrained as FG
import qualified Optimistic as O
import qualified Lazy as L
import qualified LockFree as LF

data ListType = CoarseGrained
    | FineGrained
    | Optimistic
    | Lazy
    | LockFree
    deriving (Show, Bounded, Enum)

allTypes :: [ListType]
allTypes = [(minBound :: ListType) ..]

data ThreadSafeList a =
    forall l. TSL.ThreadSafeList l a => ThreadSafeList (l a)

newEmptyList :: Ord a => ListType -> IO (ThreadSafeList a)
newEmptyList CoarseGrained = fmap ThreadSafeList CG.newEmptyList
newEmptyList FineGrained = fmap ThreadSafeList FG.newEmptyList
newEmptyList Optimistic = fmap ThreadSafeList O.newEmptyList
newEmptyList Lazy = fmap ThreadSafeList L.newEmptyList
newEmptyList LockFree = fmap ThreadSafeList LF.newEmptyList

fromList :: (Ord a, Eq a) => ListType -> [a] -> IO (ThreadSafeList a)
fromList listType list = do
    safeList <- newEmptyList listType
    forM_ list (add safeList)
    return safeList

toPureList :: ThreadSafeList a -> IO [a]
toPureList (ThreadSafeList l) = TSL.toPureList l

add :: ThreadSafeList a -> a -> IO ()
add (ThreadSafeList l) x = TSL.add l x

remove :: ThreadSafeList a -> a -> IO Bool
remove (ThreadSafeList l) x = TSL.remove l x

contains :: ThreadSafeList a -> a -> IO Bool
contains (ThreadSafeList l) x = TSL.contains l x
