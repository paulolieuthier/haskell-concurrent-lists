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
    return . FineGrainedList =<< newIORef =<< newMVar . Head =<< newMVar Null

toPureList :: FineGrainedList a -> IO [a]
toPureList (FineGrainedList head) =
    let
        go prevPtr prevNode acc = do
            let curPtr = next prevNode
            curNode <- takeMVar curPtr
            case curNode of
                Node {val = y, next = nextNode } -> do
                    putMVar prevPtr prevNode
                    go curPtr curNode (y:acc) 
                Null -> do
                    putMVar prevPtr prevNode
                    putMVar curPtr curNode
                    return $ reverse acc
    in do
        startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode []

add :: (Eq a, Ord a) => FineGrainedList a -> a -> IO Bool
add (FineGrainedList head) x =
    let
        go prevPtr prevNode = do
            let curPtr = next prevNode
            curNode <- takeMVar curPtr
            case curNode of
                Node { val = y, next = nextNode } ->
                    if (x == y) then do
                        putMVar prevPtr prevNode
                        putMVar curPtr curNode
                        return False
                    else if (x > y) then do
                        putMVar prevPtr prevNode
                        go curPtr curNode
                    else do
                        let newNode = Node { val = x, next = curPtr }
                        newPtr <- newMVar newNode
                        putMVar prevPtr (prevNode { next = newPtr })
                        putMVar curPtr curNode
                        return True
                Null -> do
                    let newNode = Node { val = x, next = curPtr }
                    newPtr <- newMVar newNode
                    putMVar prevPtr (prevNode { next = newPtr })
                    putMVar curPtr curNode
                    return True
    in do
        startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode

remove :: (Eq a, Ord a) => FineGrainedList a -> a -> IO Bool
remove (FineGrainedList head) x =
    let
        go prevPtr prevNode = do
            let curPtr = next prevNode
            curNode <- takeMVar curPtr
            case curNode of
                Node { val = y, next = nextNode } ->
                    if (x > y) then do
                        putMVar prevPtr prevNode
                        putMVar curPtr curNode
                        return False
                    else if (x < y) then do
                        putMVar prevPtr prevNode
                        go curPtr curNode
                    else do
                        putMVar prevPtr (prevNode { next = next curNode })
                        putMVar curPtr curNode
                        return True
                Null -> do
                    putMVar prevPtr prevNode
                    putMVar curPtr curNode
                    return False
    in do
        startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode

contains :: Eq a => FineGrainedList a -> a -> IO Bool
contains (FineGrainedList head) x =
    let
        go prevPtr prevNode = do
            let curPtr = next prevNode
            curNode <- takeMVar curPtr
            case curNode of
                Node {val = y, next = nextNode } ->
                    if (x == y)
                    then do
                        putMVar prevPtr prevNode
                        putMVar curPtr curNode
                        return True
                    else do
                        putMVar prevPtr prevNode
                        go curPtr curNode
                Null -> do
                    putMVar prevPtr prevNode
                    putMVar curPtr curNode
                    return False
    in do
        startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode
