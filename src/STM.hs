{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module STM
    ( toPureList
    , newEmptyList
    , add
    , remove
    , contains
    ) where

import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Data.IORef
import qualified ThreadSafeList as TSL

type Pointer a = TVar (List a)
newtype StmList a = StmList (Pointer a)

instance (Eq a, Ord a) => TSL.ThreadSafeList StmList a where
    newEmptyList = newEmptyList
    toPureList = toPureList
    add = add
    remove = remove
    contains = contains

data List a = Node { val :: a, next :: Pointer a }
    | DelNode { next :: Pointer a }
    | Head { next :: Pointer a }
    | Null
    deriving Eq

newEmptyList :: IO (StmList a)
newEmptyList = do
    head <- newTVarIO . Head =<< newTVarIO Null
    return (StmList head)

fromList :: (Eq a, Ord a) => [a] -> IO (StmList a)
fromList pureList = do
    list <- newEmptyList
    foldM addToList list pureList
    where addToList list item = add list item >> return list

toPureList :: Eq a => StmList a -> IO [a]
toPureList (StmList var) =
    let
        go prevPtr xs = do
            prevNode <- readTVar prevPtr
            let currPtr = next prevNode
            currNode <- readTVar currPtr

            case currNode of
                Node { val = val, next = nextNode } -> go currPtr (val:xs)
                DelNode { next = nextNode } -> go currPtr xs
                Null -> return . reverse $ xs
    in atomically $ go var []

updateNextPointer :: (Pointer a) -> (Pointer a) -> STM ()
updateNextPointer firstPtr newPtr = do
    node <- readTVar firstPtr
    writeTVar firstPtr (node { next = newPtr })

searchAndExecute ::
    (Eq a, Ord a)
    => StmList a
    -> a
    -> (Pointer a -> List a -> Pointer a -> List a -> Bool -> STM (IO Bool))
    -> IO Bool
searchAndExecute (StmList head) x function =
    let
        go prevPtr = runStmTransactions $ do
            prevNode <- readTVar prevPtr
            let curPtr = next prevNode
            curNode <- readTVar curPtr

            case curNode of
                DelNode { next = nextNode } ->
                    case prevNode of
                        DelNode {} -> return (go head)
                        Head {} -> do
                            writeTVar prevPtr $ Head nextNode
                            return (go prevPtr)
                        Node { val = y } -> do
                            writeTVar prevPtr $ Node y nextNode
                            return (go prevPtr)
                Null -> function prevPtr prevNode curPtr curNode False
                Node { val = y, next = nextNode } -> do
                    if x == y then function prevPtr prevNode curPtr curNode True
                    else if y > x then function prevPtr prevNode curPtr curNode False
                    else return (go curPtr)
    in go head

add :: (Eq a, Ord a) => (StmList a) -> a -> IO ()
add list x = do
    searchAndExecute list x $ \prevPtr prevNode curPtr curNode _ -> do
        newPtr <- newTVar $ Node x curPtr
        writeTVar prevPtr $ prevNode { next = newPtr }
        return $ return True
    return ()

remove :: (Eq a, Ord a) => (StmList a) -> a -> IO Bool
remove list x = searchAndExecute list x $ \prevPtr prevNode curPtr curNode itemFound -> do
    if not itemFound then return $ return False
    else do
        writeTVar curPtr (DelNode $ next curNode)
        return $ return True

contains :: (Eq a, Ord a) => (StmList a) -> a -> IO Bool
contains list x = searchAndExecute list x $ \_ _ _ _ itemFound -> return $ return itemFound

runStmTransactions :: STM (IO a) -> IO a
runStmTransactions stm = do
    io <- atomically stm
    io
