import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Criterion.Main
import Data.Foldable
import Data.Traversable
import GHC.Conc (getNumProcessors)
import System.Random
import System.Random.Shuffle

import qualified Lib as L

main :: IO ()
main = do
    let numOperations = 10000
    realisticBenchmarkData <- prepareRealisticData numOperations

    defaultMain
        [ bgroup "High-Contention" $ highContentionBenchs numOperations
        , bgroup "Realistic" $ realisticBenchs realisticBenchmarkData
        ]
    where
        highContentionBenchs numOperations =
            [ bench (show listType) $ nfIO (highContentionBench listType numOperations)
                | listType <- L.allTypes ]
        realisticBenchs benchData =
            [ bench (show listType) $ nfIO (realisticBench listType benchData)
                | listType <- L.allTypes ]

highContentionBench :: L.ListType -> Int -> IO ()
highContentionBench listType numOperations = do
    numProcessors <- getNumProcessors
    let numExtraThreads = 2
    let numAddingThreads = numProcessors - numExtraThreads

    list <- forM [1 .. numOperations] $ \_ -> randomRIO (0, 99) :: IO Int
    let originalNumberOf42 = length $ filter (== 42) list

    safeList <- L.fromList listType list

    mainLocks <- forM [1 .. numAddingThreads] $ \_ -> newEmptyMVar
    extraLocks <- forM [1 .. numExtraThreads] $ \_ -> newEmptyMVar

    forkIO $ forM_ [1 .. numOperations] $ \_ -> do
        L.add safeList 41
        putMVar (extraLocks !! 0) ()

    forkIO $ forM_ [1 .. numOperations] $ \_ -> do
        L.remove safeList 41
        putMVar (extraLocks !! 1) ()

    forM_ mainLocks $ \lock -> forkIO $ do
        forM_ [1 .. numOperations] $ \_ -> L.add safeList 42
        putMVar lock ()

    mapM_ takeMVar (mainLocks ++ extraLocks)

    pureList <- L.toPureList safeList
    return ()

prepareRealisticData :: Int -> IO ([Int], [L.ThreadSafeList Int -> IO ()])
prepareRealisticData numOperations = do
    initialElements <- forM [1 .. numOperations] $ \_ -> randomRIO (0, 9999)

    operations <- do
        let numInsertions = (numOperations * 2) `quot` 10
        let numRemovals   = (numOperations * 1) `quot` 10
        let numFinds      = (numOperations * 7) `quot` 10

        insertions <- replicateM numInsertions $ do
            rand <- randomRIO (0, 9999)
            return $ \list -> L.add list rand

        removals <- replicateM numRemovals $ do
            rand <- randomRIO (0, 9999)
            return $ \list -> L.remove list rand >> return ()

        finds <- replicateM numFinds $ do
            rand <- randomRIO (0, 9999)
            return $ \list -> L.contains list rand >> return ()

        shuffleM . concat $ [insertions, removals, finds]

    return (initialElements, operations)

realisticBench :: Ord a => L.ListType -> ([a], [L.ThreadSafeList a -> IO ()]) -> IO ()
realisticBench listType (initialElements, operations) = do
    numProcessors <- getNumProcessors
    let numOperations = length initialElements

    safeList <- L.fromList listType initialElements

    locks <- forM [1 .. numProcessors] $ \_ -> newEmptyMVar
    forM_ locks $ \lock -> forkIO $ do
        forM_ operations $ \operation -> operation safeList
        putMVar lock ()

    mapM_ takeMVar locks

    pureList <- L.toPureList safeList
    return ()
