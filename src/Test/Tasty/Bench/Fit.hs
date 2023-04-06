{-# LANGUAGE CPP #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RecordWildCards #-}

#ifdef DEBUG
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
#endif

-- | Guess complexity of the function.
module Test.Tasty.Bench.Fit (
  fit,
  mkFitConfig,
  FitConfig (..),
) where

import Control.DeepSeq (NFData)
import Test.Tasty (Timeout, mkTimeout)
import Test.Tasty.Bench (Benchmarkable, RelStDev (..), measureCpuTime, nf)
import Test.Tasty.Bench.Fit.Complexity
import Test.Tasty.Bench.Fit.Samples

#ifdef DEBUG
import Chart
import Optics.Core
#endif

-- | Configuration for 'fit'.
data FitConfig = FitConfig
  { fitBench :: Int -> Benchmarkable
  -- ^ Which function to measure? Typically 'nf' @f@.
  , fitLow :: Int
  -- ^ The smallest size of the input.
  -- It should be as small as possible, but big enough for the main asymptotic
  -- term to dwarf constant overhead and other terms.
  , fitHigh :: Int
  -- ^ The largest size of the input.
  -- As large as practically possible, at least 100x larger than
  -- the smallest size.
  , fitTimeout :: Timeout
  -- ^ Timeout of individual measurements.
  , fitRelStDev :: RelStDev
  -- ^ Target relative standard deviation of individual measurements.
  }

-- | Create a default configuration.
mkFitConfig
  :: (NFData a)
  => (Int -> a)
  -- ^ Function to measure.
  -> (Int, Int)
  -- ^ The smallest and the largest sizes of the input.
  -> FitConfig
mkFitConfig f (low, high) =
  FitConfig
    { fitBench = nf f
    , fitLow = low
    , fitHigh = high
    , fitTimeout = mkTimeout 1e8
    , fitRelStDev = RelStDev 0.05
    }

-- | Determine time complexity of the function:
--
-- * Generate a list of inputs using 'genSamples'.
-- * Measure execution time on each input using 'measureCpuTime' from @tasty-bench@.
-- * Guess the complexity using 'guessComplexity'.
--
-- >>> fit $ mkFitConfig (\x -> sum [1..x]) (10, 10000)
-- 1.2153e-8 * x
-- >>> fit $ mkFitConfig (\x -> Data.List.nub [1..x]) (10, 10000)
-- 2.8369e-9 * x ^ 2
-- >>> fit $ mkFitConfig (\x -> Data.List.sort $ take x $ iterate (\n -> n * 6364136223846793005 + 1) (1 :: Int)) (10, 10000)
-- 5.2990e-8 * x * log x
--
-- This function works best when 'fitBench' routine does not put pressure on garbage
-- collections. Otherwise GC scheduling may greatly skew observed time complexity,
-- unless 'fitLow' is large enough to even out generational GC. The latter event
-- happens earlier if 'fitBench' is made to allocate more, which can be achieved by
-- compiling with @-O0@.
--
-- When getting surprising results, consider decreasing 'fitRelStDev'.
fit :: FitConfig -> IO Complexity
fit FitConfig {..} = do
  let samples = genSamples fitLow fitHigh
  measurements <- traverse (measureCpuTime fitTimeout fitRelStDev . fitBench) samples
  let pairs = zip (map fromIntegral samples) measurements
      cmpl = guessComplexity pairs
#ifdef DEBUG
  print pairs
  let line1 = map (uncurry Point) pairs
      line2 = map (\x -> Point (fromIntegral x) (evalComplexity cmpl (fromIntegral x))) [fitLow..fitHigh]
  let styles = (\c -> defaultLineStyle & #color .~ palette1 c & #size .~ 0.002) <$> [0..1]
  let cs = zipWith (\s x -> LineChart s [x]) styles [line1, line2]
  let lineExample = mempty & #charts .~ named "line" cs & #hudOptions .~ defaultHudOptions :: ChartOptions
  writeChartOptions "tasty-bench-fit.svg" lineExample
#endif
  pure cmpl
