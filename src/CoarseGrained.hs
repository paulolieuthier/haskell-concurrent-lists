import Control.Concurrent.MVar

addToPureList :: (Ord a) => [a] -> a -> [a]
addToPureList [] a = [a]
addToPureList (x:xs) a =
    if a <= x then (a:x:xs) else (x:addToPureList xs a)

-- exception-safe
add :: (Ord a) => MVar [a] -> a -> IO ()
add listMVar x = do
    modifyMVar_ listMVar $ \list ->  return $ addToPureList list x

main :: IO ()
main = do
    list <- newMVar [1, 3, 5]
    add list 2
    add list 4
    add list 0
    add list 6
    pureList <- readMVar list
    putStrLn $ show pureList
