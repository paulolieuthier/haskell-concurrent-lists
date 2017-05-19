import Control.Concurrent.MVar
import Data.IORef

newtype ListHandle a = ListHandle (IORef (MVar (List a)))

data List a = Node { val :: a, next :: MVar (List a) }
    | Null
    | Head { next :: MVar (List a) }
    deriving Eq

instance Show a => Show (List a) where
    show Null = "Null"
    show (Head {}) = "Head"
    show node = "Node " ++ (show $ val node)

newEmptyList :: IO (ListHandle a)
newEmptyList = do
    return . ListHandle =<< newIORef =<< newMVar . Head =<< newMVar Null

toPureList :: ListHandle a -> IO [a]
toPureList (ListHandle head) =
    let go prevPtr prevNode acc = do
        let curPtr = next prevNode
        curNode <- takeMVar curPtr
        case curNode of
            Node {val = y, next = nextNode } -> do
                putMVar prevPtr prevNode
                go curPtr curNode (y:acc) 
            Null -> do
                putMVar prevPtr prevNode
                putMVar curPtr curNode
                return $ reverse acc
    in do
        startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode []

contains :: Eq a => ListHandle a -> a -> IO Bool
contains (ListHandle head) x =
    let go prevPtr prevNode = do
        let curPtr = next prevNode
        curNode <- takeMVar curPtr
        case curNode of
            Node {val = y, next = nextNode } ->
                if (x == y)
                then do
                    putMVar prevPtr prevNode
                    putMVar curPtr curNode
                    return True
                else do
                    putMVar prevPtr prevNode
                    go curPtr curNode
            Null -> do
                putMVar prevPtr prevNode
                putMVar curPtr curNode
                return False
    in do
        startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode

add :: (Eq a, Ord a) => ListHandle a -> a -> IO Bool
add (ListHandle head) x =
    let go prevPtr prevNode = do
        let curPtr = next prevNode
        curNode <- takeMVar curPtr
        case curNode of
            Node { val = y, next = nextNode } ->
                if (x == y) then do
                    putMVar prevPtr prevNode
                    putMVar curPtr curNode
                    return False
                else if (x > y) then do
                    putMVar prevPtr prevNode
                    go curPtr curNode
                else do
                    let newNode = Node { val = x, next = curPtr }
                    newPtr <- newMVar newNode
                    putMVar prevPtr (prevNode { next = newPtr })
                    putMVar curPtr curNode
                    return True
            Null -> do
                let newNode = Node { val = x, next = curPtr }
                newPtr <- newMVar newNode
                putMVar prevPtr (prevNode { next = newPtr })
                putMVar curPtr curNode
                return True
    in do
        startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode

remove :: (Eq a, Ord a) => ListHandle a -> a -> IO Bool
remove (ListHandle head) x =
    let go prevPtr prevNode = do
        let curPtr = next prevNode
        curNode <- takeMVar curPtr
        case curNode of
            Node { val = y, next = nextNode } ->
                if (x > y) then do
                    putMVar prevPtr prevNode
                    putMVar curPtr curNode
                    return False
                else if (x < y) then do
                    putMVar prevPtr prevNode
                    go curPtr curNode
                else do
                    putMVar prevPtr (prevNode { next = next curNode })
                    putMVar curPtr curNode
                    return True
            Null -> do
                putMVar prevPtr prevNode
                putMVar curPtr curNode
                return False
    in do
        startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode

main :: IO ()
main = do
    list <- newEmptyList
    add list 10
    add list 40
    remove list 10
    add list 30
    add list 20

    putStrLn . show =<< toPureList list
