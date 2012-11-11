module Test.QuickCheckSpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck

import qualified Test.QuickCheck as Q

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "exhaustive" $ do
    it "is True for a Bool" $ do
      Q.exhaustive (undefined :: Bool) `shouldBe` True

    it "is True for a Bool wrapped with property" $ do
      (Q.exhaustive . Q.property) (undefined :: Bool) `shouldBe` True

    it "is False for a predicate" $ do
      Q.exhaustive (undefined :: Int -> Bool) `shouldBe` False

    it "is False for a predicate wrapped with property" $ do
      (Q.exhaustive . Q.property) (undefined :: Int -> Bool) `shouldBe` False
