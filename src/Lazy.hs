{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Lazy
    ( newEmptyList
    , toPureList
    , add
    , remove
    , contains
    ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.IORef
import qualified ThreadSafeList as TSL

type Mark = IORef Bool
type Pointer a = IORef (List a, MVar (), Mark)
type PointerAndData a = (Pointer a, MVar ())
newtype LazyList a = LazyList (Pointer a)

instance (Eq a, Ord a) => TSL.ThreadSafeList LazyList a where
    newEmptyList = newEmptyList
    toPureList = toPureList
    add = add
    remove = remove
    contains = contains

data List a = Node { val :: a, next :: Pointer a }
    | Null
    | Head { next :: Pointer a }
    deriving Eq

newEmptyList :: IO (LazyList a)
newEmptyList = do
    null <- newIORef =<< ((,,) Null) <$> newMVar () <*> newIORef False
    head <- newIORef =<< ((,,) $ Head null) <$> newMVar () <*> newIORef False
    return $ LazyList head

toPureList :: LazyList a -> IO [a]
toPureList (LazyList head) =
    let
        go prevPtr prevNode prevMVar acc = do
            let currPtr = next prevNode
            (currNode, currMVar, currMark) <- readIORef currPtr
            takeMVar currMVar
            currMarked <- readIORef currMark
            case currNode of
                Node { val = y, next = nextNode } -> do
                    let list = if currMarked then acc else (y:acc)
                    putMVar prevMVar ()
                    go currPtr currNode currMVar list 
                Null -> do
                    putMVar prevMVar ()
                    putMVar currMVar ()
                    return $ reverse acc
    in do
        (startNode, startMVar, _) <- readIORef head
        takeMVar startMVar
        go head startNode startMVar []

validate :: Eq a => (List a) -> (Pointer a) -> Mark -> Mark -> IO Bool
validate prevNode currPtr prevMark currMark = do
    prevMarked <- readIORef prevMark
    currMarked <- readIORef currMark

    return $ (not prevMarked) && (not currMarked) && (next prevNode == currPtr)

searchUnsafely :: Ord a => Pointer a -> a -> IO (PointerAndData a, PointerAndData a)
searchUnsafely prevPtr x = do
    (prevNode, prevMVar, prevMark) <- readIORef prevPtr
    let currPtr = next prevNode
    (currNode, currMVar, currMark) <- readIORef currPtr

    let prevStuff = (prevPtr, prevMVar)
    let currStuff = (currPtr, currMVar)

    case currNode of
        Null -> return (prevStuff, currStuff)
        Node { val = y } -> do
            if y < x then searchUnsafely currPtr x
            else return (prevStuff, currStuff)

add :: (Eq a, Ord a) => LazyList a -> a -> IO ()
add list@(LazyList headPtr) x = do
    (prevStuff, currStuff) <- searchUnsafely headPtr x
    let (prevPtr, prevMVar) = prevStuff
    let (currPtr, currMVar) = currStuff

    let
        insert prevNode = do
            let newNode = Node x currPtr
            newPtr <- newIORef =<< ((,,) newNode) <$> newMVar () <*> newIORef False
            let newPredNode = prevNode { next = newPtr }
            writeIORef prevPtr =<< ((,,) newPredNode) <$> pure prevMVar <*> newIORef False

        validationAndInsertion = do
            (prevNode, _, prevMark) <- readIORef prevPtr
            (currNode, _, currMark) <- readIORef currPtr

            isValid <- validate prevNode currPtr prevMark currMark
            when isValid $ insert prevNode
            return isValid

    maybeSuccessfull <- bracket_
        (mapM_ takeMVar [prevMVar, currMVar])
        (mapM_ (flip putMVar ()) [currMVar, prevMVar])
        validationAndInsertion

    if maybeSuccessfull then return ()
    else add list x

remove :: (Eq a, Ord a) => LazyList a -> a -> IO Bool
remove list@(LazyList headPtr) x = do
    (prevStuff, currStuff) <- searchUnsafely headPtr x
    let (prevPtr, prevMVar) = prevStuff
    let (currPtr, currMVar) = currStuff

    let
        delete prevNode prevMark currNode currMark = do
            atomicWriteIORef currMark True
            let newPredNode = prevNode { next = next currNode }
            writeIORef prevPtr =<< ((,,) newPredNode) <$> pure prevMVar <*> pure prevMark

        validationAndRemoval = do
            (prevNode, _, prevMark) <- readIORef prevPtr
            (currNode, _, currMark) <- readIORef currPtr

            isValid <- validate prevNode currPtr prevMark currMark
            if not isValid then return Nothing
            else do 
                canBeAdded <- case currNode of
                    Node { val = y } ->
                        if y == x then do
                            delete prevNode prevMark currNode currMark
                            return True
                        else return False
                    Null -> return False
                return $ Just canBeAdded

    maybeSuccessfull <- bracket_
        (mapM_ takeMVar [prevMVar, currMVar])
        (mapM_ (flip putMVar ()) [currMVar, prevMVar])
        validationAndRemoval

    maybe (remove list x) return maybeSuccessfull

contains :: Ord a => LazyList a -> a -> IO Bool
contains list@(LazyList headPtr) x = do
    (headNode, _, _) <- readIORef headPtr
    maybeCurr <- findCurr (next headNode) x
    case maybeCurr of
        Nothing -> return False
        Just (currVal, currMark) -> do
            currMarked <- readIORef currMark
            return $ x == currVal && not currMarked
    where
        findCurr currPtr val = do
            (currNode, _, currMark) <- readIORef currPtr
            case currNode of
                Null -> return Nothing
                Node { val = y } ->
                    if y < x then findCurr (next currNode) val
                    else return $ Just (y, currMark)
