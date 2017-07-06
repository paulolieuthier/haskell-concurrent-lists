import Data.Foldable
import Test.Hspec
import Test.QuickCheck

import qualified Lib as L

main :: IO ()
main = hspec $ do
    forM_ L.allTypes $ \listType -> do
        describe (show listType ++ " implementation") $ do
            testInsertions listType

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

    it "should fail to insert already existing value" $ do
        safeList <- L.fromList listType [42]
        result <- L.add safeList 42
        result `shouldBe` False

        list <- L.toPureList safeList
        list `shouldBe` [42]
