import Data.Foldable
import Test.Hspec
import Test.QuickCheck

import qualified Lib as L

main :: IO ()
main = hspec $ do
    forM_ L.allTypes $ \listType -> do
        describe (show listType ++ " implementation") $ do
            testInsertions listType
            testRemovals listType
            testContains listType

testInsertions listType = do
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

    it "should not insert already existing value" $ do
        safeList <- L.fromList listType [42]
        result <- L.add safeList 42
        result `shouldBe` False

        list <- L.toPureList safeList
        list `shouldBe` [42]

testRemovals listType = do
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

testContains listType = do
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
