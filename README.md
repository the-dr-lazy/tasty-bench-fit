# tasty-bench-fit

Benchmark a given function for variable input sizes and find out its time complexity.

```haskell
> fit $ mkFitConfig (\x -> sum [1..x]) (10, 10000)
1.2153e-8 * x
> fit $ mkFitConfig (\x -> Data.List.nub [1..x]) (10, 10000)
2.8369e-9 * x ^ 2
> fit $ mkFitConfig (\x -> Data.List.sort $ take x $ iterate (\n -> n * 6364136223846793005 + 1) (1 :: Int)) (10, 10000)
5.2990e-8 * x * log x
```
