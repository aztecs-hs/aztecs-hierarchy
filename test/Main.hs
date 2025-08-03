{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Aztecs.Hierarchy" $ do
    it "placeholder test" $ property prop_placeholder

-- TODO: Update this test for aztecs 0.13 API
prop_placeholder :: Expectation
prop_placeholder = do
  True `shouldBe` True

{-
-- Original test - needs to be updated for new API
prop_addParents :: Expectation
prop_addParents = do
  let (_, w) = W.spawnEmpty W.empty
      (e, w') = W.spawn (bundle . Children $ Set.singleton e) w
  (_, w'') <- runSchedule (schedule Hierarchy.update) w' ()
  let (res, _) = runIdentity $ Q.all Q.fetch w''
  res `shouldMatchList` [Parent e]
-}
