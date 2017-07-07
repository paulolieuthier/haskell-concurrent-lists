{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module LockFree
    ( newEmptyList
    , toPureList
    , add
    , remove
    , contains
    ) where

import Data.IORef
import Data.Maybe
import Debug.Trace
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import qualified ThreadSafeList as TSL

newtype LockFreeList a = LockFreeList (IORef (List a))

instance (Eq a, Ord a) => TSL.ThreadSafeList LockFreeList a where
    newEmptyList = newEmptyList
    toPureList = toPureList
    add = add
    remove = remove
    contains = contains

data List a = Node { val :: a, next :: IORef (List a) }
    | DelNode { next :: IORef (List a) }
    | Null
    | Head { next :: IORef (List a) }
    deriving Eq

type Pred a = IORef (List a)
type Curr a = IORef (List a)

newEmptyList :: IO (LockFreeList a)
newEmptyList = return . LockFreeList =<< newIORef . Head =<< newIORef Null

toPureList :: LockFreeList a -> IO [a]
toPureList (LockFreeList headPtr) = go headPtr []
    where
        go prevPtr xs = do
            prevNode <- readIORef prevPtr
            let curPtr = next prevNode
            curNode <- readIORef curPtr
            
            case curNode of
                Node { val = y, next = nextNode } -> go curPtr (y:xs)
                Null -> return . reverse $ xs
                DelNode { next = nextNode } -> go curPtr xs

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
    atomicModifyIORef ptr $ \cur ->
        if cur == old then (new, True)
        else (cur, False)

window :: Ord a => LockFreeList a -> a -> IO (Maybe (Pred a, Curr a))
window (LockFreeList head) x = go head
    where
        go prevPtr = do
            prevNode <- readIORef prevPtr
            let curPtr = next prevNode
            curNode <- readIORef curPtr

            case curNode of
                DelNode { next = nextNode } -> do
                    case prevNode of
                        Node {} -> do
                            let newNode = Node (val prevNode) nextNode
                            b <- atomCAS prevPtr prevNode newNode
                            if (not b) then go head
                            else go curPtr
                        Head {} -> do
                            let newNode = Head nextNode
                            b <- atomCAS prevPtr prevNode newNode
                            if (not b) then go head
                            else go curPtr
                        DelNode {} -> go head
                Node { val = y, next = nextNode } ->
                    if (y >= x) then return . Just $ (prevPtr, curPtr)
                    else go curPtr
                Null -> return Nothing

add :: (Eq a, Ord a) => LockFreeList a -> a -> IO ()
add (LockFreeList head) x = go head
    where
        go prevPtr = do
            prevNode <- readIORef prevPtr

            let curPtr = next prevNode
            curNode <- readIORef curPtr

            let newNode = Node x curPtr
            newPtr <- newIORef newNode
            
            case curNode of
                Node { val = y, next = nextNode } -> do
                    if (x > y) then go curPtr
                    else do
                        let newPredNode = prevNode { next = newPtr }
                        b <- atomCAS prevPtr prevNode newPredNode
                        if b then return ()
                        else go prevPtr

                Null -> do
                    let newPredNode = prevNode { next = newPtr }
                    b <- atomCAS prevPtr prevNode newPredNode
                    if b then return ()
                    else go prevPtr

                DelNode { next = nextNode } ->
                    case prevNode of
                        Node {} -> do
                            let new = Node (val prevNode) nextNode
                            b <- atomCAS prevPtr prevNode new
                            if b then go prevPtr
                            else go head
                        Head {} -> do
                            let new = Head nextNode
                            b <- atomCAS prevPtr prevNode new
                            if b then go prevPtr
                            else go head
                        DelNode {} -> go head

remove :: (Eq a, Ord a) => LockFreeList a -> a -> IO Bool
remove list x = do
    win <- window list x
    case win of
        Nothing -> return False
        Just (prevPtr, curPtr) -> do
            curNode@(Node { val = y }) <- readIORef curPtr
            if (y /= x) then return False
            else do
                let nextPtr = next curNode
                b <- atomCAS curPtr curNode (DelNode { next = nextPtr })
                if (not b) then remove list x
                else do
                    prevNode <- readIORef prevPtr
                    case prevNode of
                        Head {} -> do
                            let newHead = Head { next = nextPtr }
                            atomCAS prevPtr prevNode newHead
                        Node { val = v } -> do
                            let newNode = Node { val = v, next = nextPtr }
                            atomCAS prevPtr prevNode newNode
                    return True

contains :: Eq a => LockFreeList a -> a -> IO Bool
contains (LockFreeList headPtr) x = go headPtr
    where
        go prevPtr = do
            prevNode <- readIORef prevPtr
            let curPtr = next prevNode
            curNode <- readIORef curPtr
            
            case curNode of
                Node { val = y, next = nextNode } ->
                    if (x == y) then return True
                    else go curPtr
                Null -> return False
                DelNode { next = nextNode } -> go curPtr
