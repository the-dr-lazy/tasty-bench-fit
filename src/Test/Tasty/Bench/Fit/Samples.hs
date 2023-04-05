-- | Generate samples from a given interval.
module Test.Tasty.Bench.Fit.Samples (
  genSamples,
) where

import Data.List (sort)
import qualified Data.List.NonEmpty as NE

-- | Generate exponentially-distributed samples from a given interval.
-- The idea is that branchmarking a function
-- on these samples should provide suitable amount of data
-- to determine asymptotic time complexity.
--
-- >>> genSamples 10 1000
-- [10,15,20,31,40,62,80,125,160,250,320,500,640,1000]
genSamples :: Int -> Int -> [Int]
genSamples low high =
  map NE.head $
    NE.group $
      sort $
        doubleUp low high <> halfDown low high

doubleUp :: Int -> Int -> [Int]
doubleUp low high =
  takeWhile (< min (maxBound `quot` 2) high) $
    iterate (* 2) $
      max 1 low

halfDown :: Int -> Int -> [Int]
halfDown low high =
  takeWhile (> max 0 low) $
    iterate (`quot` 2) $
      max 0 high
