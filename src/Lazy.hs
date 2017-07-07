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

searchUnsafely :: Ord a => Pointer a -> a -> IO (PointerAndData a, PointerAndData a)
searchUnsafely predPtr x = do
    (predNode, predMVar, predMark) <- readIORef predPtr
    let currPtr = next predNode
    (currNode, currMVar, currMark) <- readIORef currPtr

    let predStuff = (predPtr, predMVar)
    let currStuff = (currPtr, currMVar)

    case currNode of
        Null -> return (predStuff, currStuff)
        Node { val = y } -> do
            if y < x then searchUnsafely currPtr x
            else return (predStuff, currStuff)

add :: (Eq a, Ord a) => LazyList a -> a -> IO ()
add list@(LazyList headPtr) x = do
    (predStuff, currStuff) <- searchUnsafely headPtr x
    let (predPtr, predMVar) = predStuff
    let (currPtr, currMVar) = currStuff

    let
        insert predNode = do
            let newNode = Node x currPtr
            newPtr <- newIORef =<< ((,,) newNode) <$> newMVar () <*> newIORef False
            let newPredNode = predNode { next = newPtr }
            writeIORef predPtr =<< ((,,) newPredNode) <$> pure predMVar <*> newIORef False

        validationAndInsertion = do
            (predNode, _, predMark) <- readIORef predPtr
            (currNode, _, currMark) <- readIORef currPtr

            isValid <- validate predNode currPtr predMark currMark
            when isValid $ insert predNode
            return isValid

    maybeSuccessfull <- bracket_
        (mapM_ takeMVar [predMVar, currMVar])
        (mapM_ (flip putMVar ()) [currMVar, predMVar])
        validationAndInsertion

    if maybeSuccessfull then return ()
    else add list x

remove :: (Eq a, Ord a) => LazyList a -> a -> IO Bool
remove list@(LazyList headPtr) x = do
    (predStuff, currStuff) <- searchUnsafely headPtr x
    let (predPtr, predMVar) = predStuff
    let (currPtr, currMVar) = currStuff

    let
        delete predNode predMark currNode currMark = do
            atomicWriteIORef currMark True
            let newPredNode = predNode { next = next currNode }
            writeIORef predPtr =<< ((,,) newPredNode) <$> pure predMVar <*> pure predMark

        validationAndRemoval = do
            (predNode, _, predMark) <- readIORef predPtr
            (currNode, _, currMark) <- readIORef currPtr

            isValid <- validate predNode currPtr predMark currMark
            if not isValid then return Nothing
            else do 
                canBeAdded <- case currNode of
                    Node { val = y } ->
                        if y == x then do
                            delete predNode predMark currNode currMark
                            return True
                        else return False
                    Null -> return False
                return $ Just canBeAdded

    maybeSuccessfull <- bracket_
        (mapM_ takeMVar [predMVar, currMVar])
        (mapM_ (flip putMVar ()) [currMVar, predMVar])
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
