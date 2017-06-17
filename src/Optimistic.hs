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
import Data.IORef
import qualified ThreadSafeList as TSL

type Pointer a = IORef (List a, MVar ())
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
            let curPtr = next prevNode
            (curNode, curMVar) <- readIORef curPtr
            takeMVar curMVar
            case curNode of
                Node { val = y, next = nextNode } -> do
                    putMVar prevMVar ()
                    go curPtr curNode curMVar (y:acc) 
                Null -> do
                    putMVar prevMVar ()
                    putMVar curMVar ()
                    return $ reverse acc

validate :: Ord a => (Pointer a) -> (Pointer a) -> (List a) -> (Pointer a) -> IO Bool
validate headPtr predPtr predNode curPtr = go headPtr
    where
        go nodePtr = do
            (node, _) <- readIORef nodePtr

            case predNode of
                Head { next = nextPtr } -> do
                    return $ nodePtr == predPtr && nextPtr == curPtr
                Node { val = x, next = nextPtr } -> do
                    case node of
                        Null -> return False
                        Head { next = nextNodePtr } -> go nextNodePtr
                        Node { val = y, next = nextNodePtr } -> do
                            if  y > x then return False
                            else do
                                if nodePtr == predPtr
                                then return $ nextPtr == curPtr
                                else go nextNodePtr

searchUnsafely :: Ord a => Pointer a -> a -> IO (Pointer a, Pointer a)
searchUnsafely predPtr x = do
    (predNode, _) <- readIORef predPtr
    let curPtr = next predNode
    (curNode, _) <- readIORef curPtr

    case curNode of
        Null -> return (predPtr, curPtr)
        Node { val = y } -> do
            if y < x then searchUnsafely curPtr x
            else return (predPtr, curPtr)

add :: (Eq a, Ord a) => OptimisticList a -> a -> IO Bool
add list@(OptimisticList headPtr) x = do
    (predPtr, curPtr) <- searchUnsafely headPtr x
    (predNode, predMVar) <- readIORef predPtr
    (curNode, curMVar) <- readIORef curPtr

    let
        insert = do
            let newNode = Node x curPtr
            newPtr <- newIORef =<< ((,) newNode) <$> newMVar ()
            let newPredNode = predNode { next = newPtr }
            writeIORef predPtr (newPredNode, predMVar)

        validationAndInsertion = do
            isValid <- validate headPtr predPtr predNode curPtr
            if not isValid then return Nothing
            else do
                canBeAdded <- case predNode of
                    Head {} -> insert >> return True
                    Node { val = y } ->
                        if y == x then return False
                        else insert >> return True
                return $ Just canBeAdded

    maybeSuccessful <- bracket_
        (mapM_ takeMVar [predMVar, curMVar])
        (mapM_ (flip putMVar ()) [predMVar, curMVar])
        validationAndInsertion

    maybe (add list x) (return) maybeSuccessful

remove :: (Eq a, Ord a) => OptimisticList a -> a -> IO Bool
remove list@(OptimisticList headPtr) x = do
    (predPtr, curPtr) <- searchUnsafely headPtr x
    (predNode, predMVar) <- readIORef predPtr
    (curNode, curMVar) <- readIORef curPtr

    let
        validationAndRemoval = do
            isValid <- validate headPtr predPtr predNode curPtr
            if not isValid then return Nothing
            else do
                canBeRemoved <- case curNode of
                    Head {} -> delete >> return True
                    Node { val = y } ->
                        if y == x then delete >> return True
                        else return False
                return $ Just canBeRemoved

        delete = do
            let newPredNode = predNode { next = next curNode }
            writeIORef predPtr (newPredNode, predMVar)

    maybeSuccessful <- bracket_
        (mapM_ takeMVar [predMVar, curMVar])
        (mapM_ (flip putMVar ()) [predMVar, curMVar])
        validationAndRemoval

    maybe (remove list x) return maybeSuccessful

contains :: (Eq a, Ord a) => OptimisticList a -> a -> IO Bool
contains list@(OptimisticList headPtr) x = do
    (predPtr, curPtr) <- searchUnsafely headPtr x
    (predNode, predMVar) <- readIORef predPtr
    (curNode, curMVar) <- readIORef curPtr

    let
        validationAndCheck = do
            isValid <- validate headPtr predPtr predNode curPtr
            if not isValid then return Nothing
            else case curNode of
                Null -> return $ Just False
                Node { val = y } -> return . Just $ x == y

    maybeSuccessful <- bracket_
        (mapM_ takeMVar [predMVar, curMVar])
        (mapM_ (flip putMVar ()) [predMVar, curMVar])
        validationAndCheck

    maybe (contains list x) return maybeSuccessful
