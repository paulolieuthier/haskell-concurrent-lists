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

type Pointer a = IORef (List a)
newtype LockFreeList a = LockFreeList (Pointer a)

instance (Eq a, Ord a) => TSL.ThreadSafeList LockFreeList a where
    newEmptyList = newEmptyList
    toPureList = toPureList
    add = add
    remove = remove
    contains = contains

data List a = Node { val :: a, next :: Pointer a }
    | DelNode { next :: Pointer a }
    | Null
    | Head { next :: Pointer a }
    deriving Eq

newEmptyList :: IO (LockFreeList a)
newEmptyList = fmap LockFreeList . newIORef . Head =<< newIORef Null

toPureList :: LockFreeList a -> IO [a]
toPureList (LockFreeList headPtr) =
    let
        go prevPtr xs = do
            prevNode <- readIORef prevPtr
            let curPtr = next prevNode
            curNode <- readIORef curPtr

            case curNode of
                Node { val = y, next = nextNode } -> go curPtr (y:xs)
                Null -> return . reverse $ xs
                DelNode { next = nextNode } -> go curPtr xs
    in go headPtr []

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
    atomicModifyIORef ptr $ \cur ->
        if cur == old then (new, True)
        else (cur, False)

window :: Ord a => LockFreeList a -> a -> IO (Pointer a, List a, Pointer a, List a, Bool)
window (LockFreeList head) x =
    let
        go prevPtr = do
            prevNode <- readIORef prevPtr
            let currPtr = next prevNode
            currNode <- readIORef currPtr

            case currNode of
                DelNode { next = nextPtr } -> do
                    case prevNode of
                        DelNode {} -> go head
                        Head {} -> do
                            let newPrevNode = Head nextPtr
                            b <- atomCAS prevPtr prevNode newPrevNode
                            if not b then go head
                            else go currPtr
                        Node {} -> do
                            let newPrevNode = prevNode { next = nextPtr }
                            b <- atomCAS prevPtr prevNode newPrevNode
                            if not b then go head
                            else go currPtr
                Node { val = y, next = nextPtr } -> do
                    if y == x then do
                        return (prevPtr, prevNode, currPtr, currNode, True)
                    else if y > x then do
                        return (prevPtr, prevNode, currPtr, currNode, False)
                    else go currPtr
                Null -> return (prevPtr, prevNode, currPtr, currNode, False)
    in go head

add :: (Eq a, Ord a) => LockFreeList a -> a -> IO ()
add list x = do
    (prevPtr, prevNode, currPtr, currNode, _) <- window list x

    let newNode = Node x currPtr
    newPtr <- newIORef newNode
    let newPrevNode = prevNode { next = newPtr }

    b <- atomCAS prevPtr prevNode newPrevNode
    if not b then add list x
    else return ()

remove :: (Eq a, Ord a) => LockFreeList a -> a -> IO Bool
remove list x = do
    (prevPtr, prevNode, currPtr, currNode, found) <- window list x

    if not found then return False
    else do
        -- if element is found, it is not tail
        let nextPtr = next currNode

        let markedCurrNode = DelNode { next = nextPtr }
        b <- atomCAS currPtr currNode markedCurrNode
        if not b then remove list x
        else do
            let newPrevNode = prevNode { next = nextPtr }
            atomCAS prevPtr prevNode newPrevNode
            return True

contains :: (Eq a, Ord a) => LockFreeList a -> a -> IO Bool
contains (LockFreeList headPtr) x =
    let
        go prevPtr = do
            prevNode <- readIORef prevPtr
            let currPtr = next prevNode
            currNode <- readIORef currPtr

            case currNode of
                Null -> return False
                DelNode { next = nextPtr } -> go nextPtr
                Node { val = y, next = nextPtr } -> do
                    if y == x then return True
                    else if y > x then return False
                    else go currPtr
    in go headPtr

