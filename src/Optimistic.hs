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

searchUnsafely :: Ord a => Pointer a -> a -> IO (PointerAndData a, PointerAndData a)
searchUnsafely predPtr x = do
    (predNode, predMVar) <- readIORef predPtr
    let curPtr = next predNode
    (curNode, curMVar) <- readIORef curPtr

    let predStuff = (predPtr, predMVar)
    let curStuff = (curPtr, curMVar)

    case curNode of
        Null -> return (predStuff, curStuff)
        Node { val = y } -> do
            if y < x then searchUnsafely curPtr x
            else return (predStuff, curStuff)

add :: (Eq a, Ord a) => OptimisticList a -> a -> IO ()
add list@(OptimisticList headPtr) x = do
    (predStuff, curStuff) <- searchUnsafely headPtr x
    let (predPtr, predMVar) = predStuff
    let (curPtr, curMVar) = curStuff

    let
        insert predNode = do
            let newNode = Node x curPtr
            newPtr <- newIORef =<< ((,) newNode) <$> newMVar ()
            let newPredNode = predNode { next = newPtr }
            writeIORef predPtr (newPredNode, predMVar)

        validationAndInsertion = do
            (predNode, _) <- readIORef predPtr
            (curNode, _) <- readIORef curPtr

            isValid <- validate headPtr predPtr predNode curPtr
            when isValid $ insert predNode
            return isValid

    maybeSuccessful <- bracket_
        (mapM_ takeMVar [predMVar, curMVar])
        (mapM_ (flip putMVar ()) [curMVar, predMVar])
        validationAndInsertion

    if maybeSuccessful then return ()
    else add list x

remove :: (Eq a, Ord a) => OptimisticList a -> a -> IO Bool
remove list@(OptimisticList headPtr) x = do
    (predStuff, curStuff) <- searchUnsafely headPtr x
    let (predPtr, predMVar) = predStuff
    let (curPtr, curMVar) = curStuff

    let
        delete predNode curNode = do
            let newPredNode = predNode { next = next curNode }
            writeIORef predPtr (newPredNode, predMVar)

        validationAndRemoval = do
            (predNode, _) <- readIORef predPtr
            (curNode, _) <- readIORef curPtr

            isValid <- validate headPtr predPtr predNode curPtr
            if not isValid then return Nothing
            else do
                canBeRemoved <- case curNode of
                    Node { val = y } ->
                        if y == x then do
                            delete predNode curNode
                            return True
                        else return False
                    Null -> return False
                return $ Just canBeRemoved

    maybeSuccessful <- bracket_
        (mapM_ takeMVar [predMVar, curMVar])
        (mapM_ (flip putMVar ()) [curMVar, predMVar])
        validationAndRemoval

    maybe (remove list x) return maybeSuccessful

contains :: (Eq a, Ord a) => OptimisticList a -> a -> IO Bool
contains list@(OptimisticList headPtr) x = do
    (predStuff, curStuff) <- searchUnsafely headPtr x
    let (predPtr, predMVar) = predStuff
    let (curPtr, curMVar) = curStuff

    let
        validationAndCheck = do
            (predNode, _) <- readIORef predPtr
            (curNode, _) <- readIORef curPtr

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
