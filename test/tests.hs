module Main where

import Chess
import Control.Monad (void)
import Debug.Trace (traceM)
import System.Exit (exitFailure)
import Test.Hspec
import Test.QuickCheck (Arbitrary, Testable (property), choose, chooseEnum)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Exception (evaluate)


main :: IO ()
main = hspec $ do
  describe "Chess" $ do
    
    -- a unit test
    it "a4 should be empty on the starting board" $ do
      getSquare initBoard (A, Four) `shouldBe` Empty

    -- a property test (1)
    it "any square you pick should be empty, for the starting board" $
      property $
        \x -> getSquare initBoard x `shouldBe` Empty

-- (2)!
instance Arbitrary File where
  arbitrary = chooseEnum (A, H)

instance Arbitrary Rank where
  arbitrary = chooseEnum (One, Eight)
