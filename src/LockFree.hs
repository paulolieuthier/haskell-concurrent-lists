import Data.IORef
import Data.Maybe
import Debug.Trace
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

newtype ListHandle a = ListHandle { headList :: IORef (List a) }

data List a = Node { val :: a, next :: IORef (List a) }
    | DelNode { next :: IORef (List a) }
    | Null
    | Head { next :: IORef (List a) }
    deriving Eq

instance Show a => Show (List a) where
    show Null = "Null"
    show (Head {}) = "Head"
    show (DelNode {}) = "DelNode"
    show node = "Node " ++ (show $ val node)

type Pred a = IORef (List a)
type Curr a = IORef (List a)

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
    atomicModifyIORef ptr $ \cur ->
        if cur == old then (new, True)
        else (cur, False)

window :: Ord a => ListHandle a -> a -> IO (Maybe (Pred a, Curr a))
window (ListHandle { headList = head }) x =
    let go prevPtr = do
        prevNode <- readIORef prevPtr
        let curPtr = next prevNode
        curNode <- readIORef curPtr

        case curNode of
            DelNode { next = nextNode } -> do
                case prevNode of
                    Node {} -> do
                        let new = Node { val = val prevNode, next = nextNode }
                        b <- atomCAS prevPtr prevNode new
                        if (not b) then go head
                        else go curPtr
                    Head {} -> do
                        b <- atomCAS prevPtr prevNode (Head { next = nextNode })
                        if (not b) then go head
                        else go curPtr
                    DelNode {} -> go head
            Node { val = y, next = nextNode } ->
                if (y >= x) then return . Just $ (prevPtr, curPtr)
                else go curPtr
            Null -> return Nothing
    in go head

contains :: Eq a => ListHandle a -> a -> IO Bool
contains (ListHandle { headList = head }) x =
    let go prevPtr = do
        prevNode <- readIORef prevPtr
        let curPtr = next prevNode
        curNode <- readIORef curPtr
        
        case curNode of
            Node { val = y, next = nextNode } ->
                if (x == y) then return True
                else go curPtr
            Null -> return False
            DelNode { next = nextNode } -> go curPtr
    in go head

toPureList :: Eq a => ListHandle a -> IO [a]
toPureList (ListHandle { headList = head }) =
    let go prevPtr xs = do
        prevNode <- readIORef prevPtr
        let curPtr = next prevNode
        curNode <- readIORef curPtr
        
        case curNode of
            Node { val = val, next = nextNode } -> go curPtr (val:xs)
            Null -> return . reverse $ xs
            DelNode { next = nextNode } -> go curPtr xs
    in go head []

add :: (Eq a, Ord a) => ListHandle a -> a -> IO Bool
add list x = do
    win <- window list x
    case win of
        Nothing -> return False
        Just (prevPtr, curPtr) -> do
            curNode@(Node { val = y }) <- readIORef curPtr
            if (x == y) then return False
            else do
                let node = Node { val = x, next = curPtr }
                newNode <- newIORef node

                prevNode <- readIORef prevPtr
                let newPrev = case prevNode of
                        Head {} -> Head { next = newNode }
                        Node { val = v } -> Node { val = v, next = newNode }

                b <- atomCAS prevPtr prevNode newPrev
                if b then return True
                else add list x

append :: Eq a => ListHandle a -> a -> IO Bool
append (ListHandle { headList = head }) x =
    let go prevPtr = do
        prevNode <- readIORef prevPtr
        let curPtr = next prevNode
        curNode <- readIORef curPtr
        
        case curNode of
            Node { val = y, next = nextNode } -> do
                if (x == y) then return False
                else go curPtr
            Null -> do
                new <- return . Node x =<< newIORef Null
                b <- atomCAS curPtr curNode new
                if b then return True
                else go prevPtr
            DelNode { next = nextNode } ->
                case prevNode of
                    Node {} -> do
                        let new = Node { val = val prevNode, next = nextNode }
                        b <- atomCAS prevPtr prevNode new
                        if b then go prevPtr
                        else go head
                    Head {} -> do
                        b <- atomCAS prevPtr prevNode (Head { next = nextNode })
                        if b then go prevPtr
                        else go head
                    DelNode {} -> go head
    in go head

remove :: (Eq a, Ord a) => ListHandle a -> a -> IO Bool
remove list x = do
    win <- window list x
    case win of
        Nothing -> return False
        Just (prevPtr, curPtr) -> do
            curNode@(Node { val = y }) <- readIORef curPtr
            if (y /= x) then return False
            else do
                let nextPtr = next curNode
                b <- atomCAS curPtr curNode (DelNode { next = nextPtr })
                if (not b) then remove list x
                else do
                    prevNode <- readIORef prevPtr
                    case prevNode of
                        Head {} -> do
                            let newHead = Head { next = nextPtr }
                            atomCAS prevPtr prevNode newHead
                        Node { val = v } -> do
                            let newNode = Node { val = y, next = nextPtr }
                            atomCAS prevPtr prevNode newNode
                    return True

main = do
    head <- newIORef . Head =<< newIORef Null
    let list = ListHandle { headList = head } :: (ListHandle Int)

    append list 10
    append list 20
    append list 20
    append list 30
    add list 15
    remove list 20
    add list 25
    remove list 30

    {-mvar <-newMVar 0-}

    {-forM_ [0..3] $ \_ -> forkIO $ do-}
        {-results <- forM [1..1000] $ \i -> do-}
            {-threadDelay 100-}
            {-b <- append list i-}
            {-if (not b) then return . Just $ i-}
            {-else return Nothing-}
        {-putMVar mvar =<< fmap (+ 1) (takeMVar mvar)-}

    {-forM_ [4..7] $ \_ -> do-}
        {-threadDelay 100-}
        {-forkIO $ do-}
            {-results <- forM [1..1000] $ \i -> do-}
                {-b <- delete list i-}
                {-if (not b) then return . Just $ i-}
                {-else return Nothing-}
            {-putMVar mvar =<< fmap (+ 1) (takeMVar mvar)-}

    {-let wait = do-}
        {-counter <- takeMVar mvar-}
        {-if (counter /= 8) then putMVar mvar counter >> wait-}
        {-else return ()-}
    {-wait-}

    {-forM_ results $ \list -> do-}
        {-putStrLn . show $ list-}
        {-putStrLn $ show $ length $ dropWhile (\_ -> True) list-}

    putStrLn . show =<< toPureList list
