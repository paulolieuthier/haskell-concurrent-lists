import Control.Concurrent
import Control.Concurrent.MVar
import Data.Foldable
import Data.Traversable
import GHC.Conc (getNumProcessors)
import Test.Hspec
import Test.QuickCheck

import qualified Lib as L

main :: IO ()
main = hspec $ do
    forM_ L.allTypes $ \listType -> do
        describe (show listType ++ " implementation") $ do
            testSimpleInsertions listType
            testSimpleRemovals listType
            testSimpleContains listType
            testSimpleMix listType
            testSimpleParallelScenarios listType

testSimpleInsertions listType = do
    it "should insert in empty list" $ do
        safeList <- L.newEmptyList listType
        L.add safeList 42
        list <- L.toPureList safeList
        list `shouldBe` [42]

    it "should insert in end of list" $ do
        safeList <- L.fromList listType [42]
        L.add safeList 50
        list <- L.toPureList safeList
        list `shouldBe` [42, 50]

    it "should insert in beginning of list" $ do
        safeList <- L.fromList listType [42]
        L.add safeList 40
        list <- L.toPureList safeList
        list `shouldBe` [40, 42]

    it "should allow duplicating a value" $ do
        safeList <- L.fromList listType [42]
        L.add safeList 42
        L.add safeList 42

        list <- L.toPureList safeList
        list `shouldBe` [42, 42, 42]

testSimpleRemovals listType = do
    it "should not remove missing element on list" $ do
        forM_ [[], [41], [43], [41, 43]] $ \pureList -> do
            safeList <- L.fromList listType pureList

            result <- L.remove safeList 42
            result `shouldBe` False

            list <- L.toPureList safeList
            list `shouldBe` (filter (/= 42) pureList)

    it "should remove first element of list" $ do
        safeList <- L.fromList listType [1, 2, 3]

        result <- L.remove safeList 1
        result `shouldBe` True

        list <- L.toPureList safeList
        list `shouldBe` [2, 3]

    it "should remove last element of list" $ do
        safeList <- L.fromList listType [1, 2, 3]

        result <- L.remove safeList 3
        result `shouldBe` True

        list <- L.toPureList safeList
        list `shouldBe` [1, 2]

    it "should remove remove element from middle of list" $ do
        safeList <- L.fromList listType [1, 2, 3]

        result <- L.remove safeList 2
        result `shouldBe` True

        list <- L.toPureList safeList
        list `shouldBe` [1, 3]

    it "should remove duplicate values one by one" $ do
        safeList <- L.fromList listType [42, 42, 42]

        result <- L.remove safeList 42
        result `shouldBe` True
        list <- L.toPureList safeList
        list `shouldBe` [42, 42]

        result <- L.remove safeList 42
        result `shouldBe` True
        list <- L.toPureList safeList
        list `shouldBe` [42]

        result <- L.remove safeList 42
        result `shouldBe` True
        list <- L.toPureList safeList
        list `shouldBe` []

        result <- L.remove safeList 42
        result `shouldBe` False
        list <- L.toPureList safeList
        list `shouldBe` []

testSimpleContains listType = do
    it "should not find element not in list" $ do
        forM_ [[], [41], [43], [41, 43]] $ \pureList -> do
            safeList <- L.fromList listType pureList

            result <- L.contains safeList 42
            result `shouldBe` False

    it "should find element in list" $ do
        forM_ [[42], [41, 42], [42, 43], [41, 42, 43]] $ \pureList -> do
            safeList <- L.fromList listType pureList

            result <- L.contains safeList 42
            result `shouldBe` True

testSimpleMix listType = do
    it "should containt at first but not after removal" $ do
        forM_ [[], [41], [43], [41, 43]] $ \pureList -> do
            safeList <- L.fromList listType pureList

            result <- L.contains safeList 42
            result `shouldBe` False

            result <- L.add safeList 42

            result <- L.contains safeList 42
            result `shouldBe` True

            result <- L.remove safeList 42
            result `shouldBe` True

            result <- L.contains safeList 42
            result `shouldBe` False

testSimpleParallelScenarios listType = do
    it "should respect order in parallel insertions" $ do
        safeList <- L.newEmptyList listType

        let list = reverse [1 .. 1000]
        numProcessors <- getNumProcessors
        locks <- forM [1 .. numProcessors] $ \_ -> newEmptyMVar
        forM_ locks $ \lock -> forkIO $ do
            forM_ list (L.add safeList)
            putMVar lock ()

        mapM_ takeMVar locks

        pureList <- L.toPureList safeList
        pureList `shouldBe`
            (concat . (map (replicate numProcessors)) . reverse $ list)

    it "should respect order in parallel removals" $ do
        numProcessors <- getNumProcessors

        let list = [1 .. 1000]
        safeList <- L.fromList listType
            (concat . (map (replicate numProcessors)) $ list)

        locks <- forM [1 .. numProcessors] $ \_ -> newEmptyMVar
        forM_ locks $ \lock -> forkIO $ do
            forM_ list (L.remove safeList)
            putMVar lock ()

        mapM_ takeMVar locks

        pureList <- L.toPureList safeList
        pureList `shouldBe` []
