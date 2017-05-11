import Control.Concurrent.MVar
import Data.IORef

newtype ListHandle a = ListHandle (MVar (IORef (List a)))

data List a = Node { val :: a, next :: IORef (List a) }
    | Null
    | Head { next :: IORef (List a) }
    deriving Eq

instance Show a => Show (List a) where
    show Null = "Null"
    show (Head {}) = "Head"
    show node = "Node " ++ (show $ val node)

toPureList :: Eq a => ListHandle a -> IO [a]
toPureList (ListHandle mvar) =
    let go prevPtr xs = do
        prevNode <- readIORef prevPtr
        let curPtr = next prevNode
        curNode <- readIORef curPtr
        
        case curNode of
            Node { val = val, next = nextNode } -> go curPtr (val:xs)
            Null -> return . reverse $ xs
    in withMVar mvar $ \head -> go head []

newEmptyList :: IO (ListHandle a)
newEmptyList = do
    return . ListHandle =<< newMVar =<< newIORef . Head =<< newIORef Null

updateNextPointer :: (IORef (List a)) -> (IORef (List a)) -> IO ()
updateNextPointer firstPtr newPtr = do
    node <- readIORef firstPtr
    writeIORef firstPtr (node { next = newPtr })

add :: (Eq a, Ord a) => (ListHandle a) -> a -> IO ()
add (ListHandle mvar) x =
    let go prevPtr = do
        prevNode <- readIORef prevPtr

        let curPtr = next prevNode
        curNode <- readIORef curPtr

        let newNode = Node { val = x, next = curPtr }
        newPtr <- newIORef newNode

        case curNode of
            Null -> updateNextPointer prevPtr newPtr
            Node { val = y } -> if x < y then updateNextPointer prevPtr newPtr else go curPtr
            _ -> go curPtr

    in withMVar mvar $ \head -> go head 

main :: IO ()
main = do
    list <- newEmptyList

    add list 10
    add list 40
    add list 30
    add list 20

    putStrLn . show =<< toPureList list
