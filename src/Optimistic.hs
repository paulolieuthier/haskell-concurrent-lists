import Control.Concurrent.MVar
import Control.Exception
import Data.IORef

type Pointer a = IORef (List a, MVar ())
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
    null <- newIORef =<< ((,) Null) <$> newMVar ()
    head <- newIORef =<< ((,) $ Head null) <$> newMVar ()
    return $ ListHandle head

toPureList :: ListHandle a -> IO [a]
toPureList (ListHandle head) =
    let go prevPtr prevNode prevMVar acc = do
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
    in do
        (startNode, startMVar) <- readIORef head
        takeMVar startMVar
        go head startNode startMVar []

validate :: Ord a => (Pointer a) -> (Pointer a) -> (List a) -> (Pointer a) -> IO Bool
validate headPtr predPtr predNode curPtr =
    let go nodePtr = do
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
                            if nodePtr == predPtr then return $ nextPtr == curPtr
                            else go nextNodePtr
    in go headPtr

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

contains :: (Eq a, Ord a) => ListHandle a -> a -> IO Bool
contains list@(ListHandle headPtr) x = do
    (predPtr, curPtr) <- searchUnsafely headPtr x
    (predNode, predMVar) <- readIORef predPtr
    (curNode, curMVar) <- readIORef curPtr
    
    let validationAndInsertion = do
        isValid <- validate headPtr predPtr predNode curPtr
        if isValid then do
            let newNode = Node x curPtr
            newPtr <- newIORef =<< ((,) newNode) <$> newMVar ()
            writeIORef predPtr ((predNode { next = newPtr }, predMVar))
            return True
        else add list x 

    bracket
        (mapM_ takeMVar [predMVar, curMVar])
        (\_ -> mapM_ (flip putMVar ()) [predMVar, curMVar])
        (\_ -> case curNode of
            Node { val = y } -> do
                isValid <- validate headPtr predPtr predNode curPtr
                return $ isValid && x == y
            Null -> return False)

add :: (Eq a, Ord a) => ListHandle a -> a -> IO Bool
add list@(ListHandle headPtr) x = do
    (predPtr, curPtr) <- searchUnsafely headPtr x
    (predNode, predMVar) <- readIORef predPtr
    (curNode, curMVar) <- readIORef curPtr

    let insert predNode predPtr = do
        let newNode = Node x curPtr
        newPtr <- newIORef =<< ((,) newNode) <$> newMVar ()
        let newPredNode = predNode { next = newPtr }
        writeIORef predPtr (newPredNode, predMVar)
    
    let validationAndInsertion = do
        isValid <- validate headPtr predPtr predNode curPtr
        if not isValid then return Nothing
        else do
            canBeAdded <- case predNode of
                Head {} -> insert predNode predPtr >> return True
                Node { val = y } ->
                    if y == x then return False
                    else insert predNode predPtr >> return True
            return $ Just canBeAdded

    maybeSuccessful <- bracket_
        (mapM_ takeMVar [predMVar, curMVar])
        (mapM_ (flip putMVar ()) [predMVar, curMVar])
        validationAndInsertion

    maybe (add list x) (return) maybeSuccessful

remove :: (Eq a, Ord a) => ListHandle a -> a -> IO Bool
remove list@(ListHandle headPtr) x = do
    (predPtr, curPtr) <- searchUnsafely headPtr x
    (predNode, predMVar) <- readIORef predPtr
    (curNode, curMVar) <- readIORef curPtr

    let delete = do
        let newPredNode = predNode { next = next curNode }
        writeIORef predPtr (newPredNode, predMVar)
    
    let validationAndRemoval = do
        isValid <- validate headPtr predPtr predNode curPtr
        if not isValid then return Nothing
        else do
            canBeRemoved <- case curNode of
                Head {} -> delete >> return True
                Node { val = y } ->
                    if y == x then delete >> return True
                    else return False
            return $ Just canBeRemoved

    maybeSuccessful <- bracket_
        (mapM_ takeMVar [predMVar, curMVar])
        (mapM_ (flip putMVar ()) [predMVar, curMVar])
        validationAndRemoval

    maybe (remove list x) return maybeSuccessful

main :: IO ()
main = do
    list <- newEmptyList
    putStrLn . show =<< contains list 40
    add list 40
    putStrLn . show =<< contains list 40
    remove list 30
    add list 30
    putStrLn . show =<< contains list 20
    add list 20
    remove list 20
    add list 50
    putStrLn . show =<< contains list 50
    remove list 40

    putStrLn . show =<< toPureList list
