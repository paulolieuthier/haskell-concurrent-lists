import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.IORef

type Mark = IORef Bool
type Pointer a = IORef (List a, MVar (), Mark)
newtype ListHandle a = ListHandle (Pointer a)

data List a = Node { val :: a, next :: Pointer a }
    | Null
    | Head { next :: Pointer a }
    deriving Eq

instance Show a => Show (List a) where
    show Null = "Null"
    show (Head {}) = "Head"
    show node = "Node " ++ (show $ val node)

newEmptyList :: IO (ListHandle a)
newEmptyList = do
    null <- newIORef =<< ((,,) Null) <$> newMVar () <*> newIORef False
    head <- newIORef =<< ((,,) $ Head null) <$> newMVar () <*> newIORef False
    return $ ListHandle head

toPureList :: ListHandle a -> IO [a]
toPureList (ListHandle head) =
    let go predPtr predNode predMVar acc = do
        let currPtr = next predNode
        (curNode, curMVar, currMark) <- readIORef currPtr
        takeMVar curMVar
        currMarked <- readIORef currMark
        case curNode of
            Node { val = y, next = nextNode } -> do
                let list = if currMarked then acc else (y:acc)
                putMVar predMVar ()
                go currPtr curNode curMVar list 
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
    (curNode, _, _) <- readIORef currPtr

    case curNode of
        Null -> return (predPtr, currPtr)
        Node { val = y } -> do
            if y < x then searchUnsafely currPtr x
            else return (predPtr, currPtr)

add :: (Eq a, Ord a) => ListHandle a -> a -> IO Bool
add list@(ListHandle headPtr) x = do
    (predPtr, currPtr) <- searchUnsafely headPtr x
    (predNode, predMVar, predMark) <- readIORef predPtr
    (curNode, curMVar, currMark) <- readIORef currPtr

    let insert predNode predPtr = do
        let newNode = Node x currPtr
        -- let newPredNode = (predNode { next = newPtr })
        newPtr <- newIORef =<< ((,,) newNode) <$> newMVar () <*> newIORef False
        writeIORef predPtr =<< ((,,) (predNode { next = newPtr })) <$> pure predMVar <*> newIORef False
        return True

    let validationAndInsertion = do
        isValid <- validate predNode currPtr predMark currMark
        if not isValid then return False
        else case predNode of
            Head {} -> insert predNode predPtr
            Node { val = y } ->
                if y == x then return False
                else insert predNode predPtr

    bracket
        (mapM_ takeMVar [predMVar, curMVar])
        (\_ -> mapM_ (flip putMVar ()) [predMVar, curMVar])
        (\_ -> case curNode of
            Node { val = y } ->
                if y == x then return False
                else validationAndInsertion
            Null -> validationAndInsertion)

main :: IO ()
main = do
    list <- newEmptyList
    add list 40
    add list 30
    add list 20
    add list 50

    putStrLn . show =<< toPureList list
