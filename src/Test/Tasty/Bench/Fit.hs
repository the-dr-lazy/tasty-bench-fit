{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RecordWildCards #-}

-- | Guess complexity of the function.
module Test.Tasty.Bench.Fit (
  fit,
  mkFitConfig,
  FitConfig (..),
) where

import Control.DeepSeq (NFData)
import Test.Tasty (Timeout, mkTimeout)
import Test.Tasty.Bench (Benchmarkable, RelStDev (..), measureCpuTime, nf)
import Test.Tasty.Bench.Fit.Complexity (Complexity, guessComplexity)
import Test.Tasty.Bench.Fit.Samples (genSamples)

-- | Configuration for 'fit'.
data FitConfig = FitConfig
  { fitBench :: Int -> Benchmarkable
  -- ^ Which function to measure? Typically 'nf' @f@.
  , fitLow :: Int
  -- ^ The smallest size of the input.
  , fitHigh :: Int
  -- ^ The largest size of the input.
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
    , fitTimeout = mkTimeout 1e7
    , fitRelStDev = RelStDev 0.05
    }

-- | Determine time complexity of the function:
--
-- * Generate a list of inputs using 'genSamples'.
-- * Measure execution time on each input using 'measureCpuTime' from @tasty-bench@.
-- * Guess the complexity using 'guessComplexity'.
--
-- >>> fit $ mkFitConfig (\x -> sum [1 .. 100 * x]) (1, 10)
-- 5.8723e-8 * x
fit :: FitConfig -> IO Complexity
fit FitConfig {..} = do
  let samples = genSamples fitLow fitHigh
  measurements <- traverse (measureCpuTime fitTimeout fitRelStDev . fitBench) samples
  -- print (zip (map fromIntegral samples) measurements)
  pure $ guessComplexity (zip (map fromIntegral samples) measurements)
