{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module FineGrained
    ( newEmptyList
    , toPureList
    , add
    , remove
    , contains
    ) where

import Control.Concurrent.MVar
import Data.IORef
import qualified ThreadSafeList as TSL

newtype FineGrainedList a = FineGrainedList (IORef (MVar (List a)))

instance (Eq a, Ord a) => TSL.ThreadSafeList FineGrainedList a where
    newEmptyList = newEmptyList
    toPureList = toPureList
    add = add
    remove = remove
    contains = contains

data List a = Node { val :: a, next :: MVar (List a) }
    | Null
    | Head { next :: MVar (List a) }
    deriving Eq

newEmptyList :: IO (FineGrainedList a)
newEmptyList = do
    fmap FineGrainedList . newIORef =<< newMVar . Head =<< newMVar Null

toPureList :: FineGrainedList a -> IO [a]
toPureList (FineGrainedList head) =
    let
        go prevPtr prevNode acc = do
            let currPtr = next prevNode
            currNode <- takeMVar currPtr
            case currNode of
                Node {val = y, next = nextNode } -> do
                    putMVar prevPtr prevNode
                    go currPtr currNode (y:acc) 
                Null -> do
                    putMVar prevPtr prevNode
                    putMVar currPtr currNode
                    return $ reverse acc
    in do
        startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode []

add :: (Eq a, Ord a) => FineGrainedList a -> a -> IO ()
add (FineGrainedList head) x =
    let
        go prevPtr prevNode = do
            let currPtr = next prevNode
            currNode <- takeMVar currPtr
            case currNode of
                Node { val = y, next = nextNode } ->
                    if (x > y) then do
                        putMVar prevPtr prevNode
                        go currPtr currNode
                    else do
                        let newNode = Node { val = x, next = currPtr }
                        newPtr <- newMVar newNode
                        putMVar prevPtr (prevNode { next = newPtr })
                        putMVar currPtr currNode
                Null -> do
                    let newNode = Node { val = x, next = currPtr }
                    newPtr <- newMVar newNode
                    putMVar prevPtr (prevNode { next = newPtr })
                    putMVar currPtr currNode
    in do
        startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode

remove :: (Eq a, Ord a) => FineGrainedList a -> a -> IO Bool
remove (FineGrainedList head) x =
    let
        go prevPtr prevNode = do
            let currPtr = next prevNode
            currNode <- takeMVar currPtr
            case currNode of
                Node { val = y, next = nextNode } ->
                    if (x < y) then do
                        putMVar currPtr currNode
                        putMVar prevPtr prevNode
                        return False
                    else if (x > y) then do
                        putMVar prevPtr prevNode
                        go currPtr currNode
                    else do
                        putMVar currPtr currNode
                        putMVar prevPtr (prevNode { next = next currNode })
                        return True
                Null -> do
                    putMVar prevPtr prevNode
                    putMVar currPtr currNode
                    return False
    in do
        startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode

contains :: Eq a => FineGrainedList a -> a -> IO Bool
contains (FineGrainedList head) x =
    let
        go prevPtr prevNode = do
            let currPtr = next prevNode
            currNode <- takeMVar currPtr
            case currNode of
                Node {val = y, next = nextNode } ->
                    if (x == y)
                    then do
                        putMVar prevPtr prevNode
                        putMVar currPtr currNode
                        return True
                    else do
                        putMVar prevPtr prevNode
                        go currPtr currNode
                Null -> do
                    putMVar prevPtr prevNode
                    putMVar currPtr currNode
                    return False
    in do
        startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode
