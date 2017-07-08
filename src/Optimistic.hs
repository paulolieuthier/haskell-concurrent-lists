{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Optimistic
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

type Pointer a = IORef (List a, MVar ())
type PointerAndData a = (Pointer a, MVar ())
newtype OptimisticList a = OptimisticList (Pointer a)

instance (Eq a, Ord a) => TSL.ThreadSafeList OptimisticList a where
    newEmptyList = newEmptyList
    toPureList = toPureList
    add = add
    remove = remove
    contains = contains

data List a = Node { val :: a, next :: Pointer a }
    | Null
    | Head { next :: Pointer a }
    deriving Eq

newEmptyList :: IO (OptimisticList a)
newEmptyList = do
    null <- newIORef =<< ((,) Null) <$> newMVar ()
    head <- newIORef =<< ((,) $ Head null) <$> newMVar ()
    return $ OptimisticList head

toPureList :: OptimisticList a -> IO [a]
toPureList (OptimisticList head) = do
    (startNode, startMVar) <- readIORef head
    takeMVar startMVar
    go head startNode startMVar []
    where
        go prevPtr prevNode prevMVar acc = do
            let currPtr = next prevNode
            (currNode, currMVar) <- readIORef currPtr
            takeMVar currMVar
            case currNode of
                Node { val = y, next = nextNode } -> do
                    putMVar prevMVar ()
                    go currPtr currNode currMVar (y:acc) 
                Null -> do
                    putMVar prevMVar ()
                    putMVar currMVar ()
                    return $ reverse acc

validate :: Ord a => (Pointer a) -> (Pointer a) -> (List a) -> (Pointer a) -> IO Bool
validate headPtr prevPtr prevNode currPtr = go headPtr
    where
        go nodePtr = do
            (node, _) <- readIORef nodePtr

            case prevNode of
                Head { next = nextPtr } -> do
                    return $ nodePtr == prevPtr && nextPtr == currPtr
                Node { val = x, next = nextPtr } -> do
                    case node of
                        Null -> return False
                        Head { next = nextNodePtr } -> go nextNodePtr
                        Node { val = y, next = nextNodePtr } -> do
                            if  y > x then return False
                            else do
                                if nodePtr == prevPtr
                                then return $ nextPtr == currPtr
                                else go nextNodePtr

searchUnsafely :: Ord a => Pointer a -> a -> IO (PointerAndData a, PointerAndData a)
searchUnsafely prevPtr x = do
    (prevNode, prevMVar) <- readIORef prevPtr
    let currPtr = next prevNode
    (currNode, currMVar) <- readIORef currPtr

    let prevStuff = (prevPtr, prevMVar)
    let currStuff = (currPtr, currMVar)

    case currNode of
        Null -> return (prevStuff, currStuff)
        Node { val = y } -> do
            if y < x then searchUnsafely currPtr x
            else return (prevStuff, currStuff)

add :: (Eq a, Ord a) => OptimisticList a -> a -> IO ()
add list@(OptimisticList headPtr) x = do
    (prevStuff, currStuff) <- searchUnsafely headPtr x
    let (prevPtr, prevMVar) = prevStuff
    let (currPtr, currMVar) = currStuff

    let
        insert prevNode = do
            let newNode = Node x currPtr
            newPtr <- newIORef =<< ((,) newNode) <$> newMVar ()
            let newPredNode = prevNode { next = newPtr }
            writeIORef prevPtr (newPredNode, prevMVar)

        validationAndInsertion = do
            (prevNode, _) <- readIORef prevPtr
            (currNode, _) <- readIORef currPtr

            isValid <- validate headPtr prevPtr prevNode currPtr
            when isValid $ insert prevNode
            return isValid

    maybeSuccessful <- bracket_
        (mapM_ takeMVar [prevMVar, currMVar])
        (mapM_ (flip putMVar ()) [currMVar, prevMVar])
        validationAndInsertion

    if maybeSuccessful then return ()
    else add list x

remove :: (Eq a, Ord a) => OptimisticList a -> a -> IO Bool
remove list@(OptimisticList headPtr) x = do
    (prevStuff, currStuff) <- searchUnsafely headPtr x
    let (prevPtr, prevMVar) = prevStuff
    let (currPtr, currMVar) = currStuff

    let
        delete prevNode currNode = do
            let newPredNode = prevNode { next = next currNode }
            writeIORef prevPtr (newPredNode, prevMVar)

        validationAndRemoval = do
            (prevNode, _) <- readIORef prevPtr
            (currNode, _) <- readIORef currPtr

            isValid <- validate headPtr prevPtr prevNode currPtr
            if not isValid then return Nothing
            else do
                canBeRemoved <- case currNode of
                    Node { val = y } ->
                        if y == x then do
                            delete prevNode currNode
                            return True
                        else return False
                    Null -> return False
                return $ Just canBeRemoved

    maybeSuccessful <- bracket_
        (mapM_ takeMVar [prevMVar, currMVar])
        (mapM_ (flip putMVar ()) [currMVar, prevMVar])
        validationAndRemoval

    maybe (remove list x) return maybeSuccessful

contains :: (Eq a, Ord a) => OptimisticList a -> a -> IO Bool
contains list@(OptimisticList headPtr) x = do
    (prevStuff, currStuff) <- searchUnsafely headPtr x
    let (prevPtr, prevMVar) = prevStuff
    let (currPtr, currMVar) = currStuff

    let
        validationAndCheck = do
            (prevNode, _) <- readIORef prevPtr
            (currNode, _) <- readIORef currPtr

            isValid <- validate headPtr prevPtr prevNode currPtr
            if not isValid then return Nothing
            else case currNode of
                Null -> return $ Just False
                Node { val = y } -> return . Just $ x == y

    maybeSuccessful <- bracket_
        (mapM_ takeMVar [prevMVar, currMVar])
        (mapM_ (flip putMVar ()) [prevMVar, currMVar])
        validationAndCheck

    maybe (contains list x) return maybeSuccessful
