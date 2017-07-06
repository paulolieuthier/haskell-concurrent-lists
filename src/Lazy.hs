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
        go predPtr predNode predMVar acc = do
            let currPtr = next predNode
            (currNode, curMVar, currMark) <- readIORef currPtr
            takeMVar curMVar
            currMarked <- readIORef currMark
            case currNode of
                Node { val = y, next = nextNode } -> do
                    let list = if currMarked then acc else (y:acc)
                    putMVar predMVar ()
                    go currPtr currNode curMVar list 
                Null -> do
                    putMVar predMVar ()
                    putMVar curMVar ()
                    return $ reverse acc
    in do
        (startNode, startMVar, _) <- readIORef head
        takeMVar startMVar
        go head startNode startMVar []

validate :: Eq a => (List a) -> (Pointer a) -> Mark -> Mark -> IO Bool
validate predNode currPtr predMark currMark = do
    predMarked <- readIORef predMark
    currMarked <- readIORef currMark

    return $ (not predMarked) && (not currMarked) && (next predNode == currPtr)

searchUnsafely :: Ord a => Pointer a -> a -> IO (Pointer a, Pointer a)
searchUnsafely predPtr x = do
    (predNode, _, _) <- readIORef predPtr
    let currPtr = next predNode
    (currNode, _, _) <- readIORef currPtr

    case currNode of
        Null -> return (predPtr, currPtr)
        Node { val = y } -> do
            if y < x then searchUnsafely currPtr x
            else return (predPtr, currPtr)

add :: (Eq a, Ord a) => LazyList a -> a -> IO Bool
add list@(LazyList headPtr) x = do
    (predPtr, currPtr) <- searchUnsafely headPtr x
    (predNode, predMVar, predMark) <- readIORef predPtr
    (currNode, curMVar, currMark) <- readIORef currPtr

    let
        insert = do
            let newNode = Node x currPtr
            newPtr <- newIORef =<< ((,,) newNode) <$> newMVar () <*> newIORef False
            let newPredNode = predNode { next = newPtr }
            writeIORef predPtr =<< ((,,) newPredNode) <$> pure predMVar <*> newIORef False

        validationAndInsertion = do
            isValid <- validate predNode currPtr predMark currMark
            if not isValid then return Nothing
            else do 
                canBeAdded <- case currNode of
                    Node { val = y } ->
                        if y == x then return False
                        else insert >> return True
                    Null {} -> insert >> return True
                return $ Just canBeAdded

    maybeSuccessfull <- bracket_
        (mapM_ takeMVar [predMVar, curMVar])
        (mapM_ (flip putMVar ()) [predMVar, curMVar])
        validationAndInsertion

    maybe (add list x) return maybeSuccessfull

remove :: (Eq a, Ord a) => LazyList a -> a -> IO Bool
remove list@(LazyList headPtr) x = do
    (predPtr, currPtr) <- searchUnsafely headPtr x
    (predNode, predMVar, predMark) <- readIORef predPtr
    (currNode, curMVar, currMark) <- readIORef currPtr

    let
        delete = do
            atomicWriteIORef currMark True
            let newPredNode = predNode { next = next currNode }
            writeIORef predPtr =<< ((,,) newPredNode) <$> pure predMVar <*> pure predMark

        validationAndRemoval = do
            isValid <- validate predNode currPtr predMark currMark
            if not isValid then return Nothing
            else do 
                canBeAdded <- case predNode of
                    Head {} -> delete >> return True
                    Node { val = y } ->
                        if y == x then return False
                        else delete >> return True
                return $ Just canBeAdded

    maybeSuccessfull <- bracket_
        (mapM_ takeMVar [predMVar, curMVar])
        (mapM_ (flip putMVar ()) [predMVar, curMVar])
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
