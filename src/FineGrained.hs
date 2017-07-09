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

type Pointer a = IORef (MVar (List a))
newtype FineGrainedList a = FineGrainedList (Pointer a)

instance (Eq a, Ord a) => TSL.ThreadSafeList FineGrainedList a where
    newEmptyList = newEmptyList
    toPureList = toPureList
    add = add
    remove = remove
    contains = contains

data List a = Node { val :: a, next :: Pointer a }
    | Null
    | Head { next :: Pointer a }
    deriving Eq

newEmptyList :: IO (FineGrainedList a)
newEmptyList = do
    null <- newIORef =<< newMVar Null
    head <- newIORef =<< (newMVar $ Head null)
    return $ FineGrainedList head

toPureList :: FineGrainedList a -> IO [a]
toPureList (FineGrainedList headPtr) =
    let
        go prevMVar prevNode acc = do
            let currPtr = next prevNode
            currMVar <- readIORef currPtr
            currNode <- takeMVar currMVar
            case currNode of
                Node { val = y, next = nextPtr } -> do
                    nextMVar <- readIORef nextPtr
                    putMVar prevMVar prevNode
                    go currMVar currNode (y:acc) 
                Null -> do
                    putMVar prevMVar prevNode
                    putMVar currMVar currNode
                    return $ reverse acc
    in do
        headMVar <- readIORef headPtr
        headNode <- takeMVar headMVar
        go headMVar headNode []

add :: (Eq a, Ord a) => FineGrainedList a -> a -> IO ()
add (FineGrainedList headPtr) x =
    let
        go prevMVar prevNode = do
            let currPtr = next prevNode
            currMVar <- readIORef currPtr
            currNode <- takeMVar currMVar
            case currNode of
                Node { val = y } ->
                    if (x > y) then do
                        putMVar prevMVar prevNode
                        go currMVar currNode
                    else do
                        let newNode = Node { val = x, next = currPtr }
                        newPtr <- newIORef =<< newMVar newNode
                        putMVar prevMVar (prevNode { next = newPtr })
                        putMVar currMVar currNode
                Null -> do
                    let newNode = Node { val = x, next = currPtr }
                    newPtr <- newIORef =<< newMVar newNode
                    putMVar prevMVar (prevNode { next = newPtr })
                    putMVar currMVar currNode
    in do
        headMVar <- readIORef headPtr
        headNode <- takeMVar headMVar
        go headMVar headNode

remove :: (Eq a, Ord a) => FineGrainedList a -> a -> IO Bool
remove (FineGrainedList headPtr) x =
    let
        go prevMVar prevNode = do
            let currPtr = next prevNode
            currMVar <- readIORef currPtr
            currNode <- takeMVar currMVar
            case currNode of
                Node { val = y } ->
                    if (x < y) then do
                        putMVar currMVar currNode
                        putMVar prevMVar prevNode
                        return False
                    else if (x > y) then do
                        putMVar prevMVar prevNode
                        go currMVar currNode
                    else do
                        putMVar currMVar currNode
                        putMVar prevMVar (prevNode { next = next currNode })
                        return True
                Null -> do
                    putMVar prevMVar prevNode
                    putMVar currMVar currNode
                    return False
    in do
        headMVar <- readIORef headPtr
        headNode <- takeMVar headMVar
        go headMVar headNode

contains :: Eq a => FineGrainedList a -> a -> IO Bool
contains (FineGrainedList headPtr) x =
    let
        go prevMVar prevNode = do
            let currPtr = next prevNode
            currMVar <- readIORef currPtr
            currNode <- takeMVar currMVar
            case currNode of
                Node { val = y } ->
                    if (x == y)
                    then do
                        putMVar prevMVar prevNode
                        putMVar currMVar currNode
                        return True
                    else do
                        putMVar prevMVar prevNode
                        go currMVar currNode
                Null -> do
                    putMVar prevMVar prevNode
                    putMVar currMVar currNode
                    return False
    in do
        headMVar <- readIORef headPtr
        headNode <- takeMVar headMVar
        go headMVar headNode
