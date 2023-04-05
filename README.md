# tasty-bench-fit

Benchmark a given function for variable input sizes and find out its time complexity.

```haskell
> fit $ mkFitConfig (\x -> sum [1 .. 100 * x]) (1, 10)
5.8723e-8 * x
```
