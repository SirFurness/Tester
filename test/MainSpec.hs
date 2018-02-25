module MainSpec (spec) where

import Test.Hspec
import Everything
import System.Random
import Control.Monad.State

spec :: Spec
spec = do
  describe "sumOfGeoSeries" $ do
    it "returns correct text" $
      (evalState sumOfGeoSeries $ mkStdGen 0)  `shouldBe` "Find the sum of the first 4 numbers in the geometric series if a1 = 11 and r = 0.64"

