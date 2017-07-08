{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CoarseGrained
    ( toPureList
    , newEmptyList
    , add
    , remove
    , contains
    ) where

import Control.Concurrent.MVar
import Data.IORef
import qualified ThreadSafeList as TSL

type Pointer a = IORef (List a)
newtype CoarseGrainedList a = CoarseGrainedList (MVar (Pointer a))

instance (Eq a, Ord a) => TSL.ThreadSafeList CoarseGrainedList a where
    newEmptyList = newEmptyList
    toPureList = toPureList
    add = add
    remove = remove
    contains = contains

data List a = Node { val :: a, next :: Pointer a }
    | Null
    | Head { next :: Pointer a }
    deriving Eq

newEmptyList :: IO (CoarseGrainedList a)
newEmptyList = do
    fmap CoarseGrainedList . newMVar =<< newIORef . Head =<< newIORef Null

toPureList :: Eq a => CoarseGrainedList a -> IO [a]
toPureList (CoarseGrainedList mvar) =
    let
        go prevPtr xs = do
            prevNode <- readIORef prevPtr
            let curPtr = next prevNode
            curNode <- readIORef curPtr
            
            case curNode of
                Node { val = val, next = nextNode } -> go curPtr (val:xs)
                Null -> return . reverse $ xs
    in withMVar mvar $ \head -> go head []

updateNextPointer :: (Pointer a) -> (Pointer a) -> IO ()
updateNextPointer firstPtr newPtr = do
    node <- readIORef firstPtr
    writeIORef firstPtr (node { next = newPtr })

add :: (Ord a) => (CoarseGrainedList a) -> a -> IO ()
add (CoarseGrainedList mvar) x =
    let
        go prevPtr = do
            prevNode <- readIORef prevPtr

            let curPtr = next prevNode
            curNode <- readIORef curPtr

            let newNode = Node { val = x, next = curPtr }
            newPtr <- newIORef newNode

            case curNode of
                Null -> updateNextPointer prevPtr newPtr
                Node { val = y } ->
                    if x <= y then updateNextPointer prevPtr newPtr
                    else go curPtr
                _ -> go curPtr

    in withMVar mvar go

remove :: (Eq a) => (CoarseGrainedList a) -> a -> IO Bool
remove (CoarseGrainedList mvar) x =
    let
        go prevPtr = do
            prevNode <- readIORef prevPtr

            let curPtr = next prevNode
            curNode <- readIORef curPtr

            case curNode of
                Null -> return False
                Node { val = y, next = nextPtr } ->
                    if x == y then updateNextPointer prevPtr nextPtr >> return True
                    else go curPtr
                _ -> go curPtr

    in withMVar mvar go

contains :: (Eq a) => (CoarseGrainedList a) -> a -> IO Bool
contains (CoarseGrainedList mvar) x =
    let
        go prevPtr = do
            prevNode <- readIORef prevPtr

            case prevNode of
                Null -> return False
                Head { next = nextPtr } -> go nextPtr
                Node { val = y, next = nextPtr } ->
                    if x == y then return True
                    else go nextPtr

    in withMVar mvar go
