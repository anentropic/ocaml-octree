# Benchmark

OCaml 4.14  
Macbook Air M1 16GB macOS 13.0

Made using https://github.com/janestreet/core_bench

See https://blog.janestreet.com/core_bench-micro-benchmarking-for-ocaml/ for more details.

```shell
dune build
_build/default/benchmarks/nearest.exe -quota 3 -stabilize-gc | benchmarks/parsebench
```

### "Original implementation"

Commit [d1f6ca1](https://github.com/anentropic/ocaml-oktree/tree/d1f6ca1/)

Using `Gg.V3` as the point type.

Testing `Oktree.nearest` method.

## 2022-11-14

This is an updated version of the benchmark (see [2022-11-12](nearest-2022-11-12-original-implementation.md))

The code under test is the same, but the benchmarking code has been modified.

- In original benchmark, each `Uniform dist/pts:256` case used a different points set
  - Updated so they share same point set - should make results more comparable
- Still quite variable across runs... particularly the `pts:256` cases. I interpreted this as due to sometimes you would just randomly get a point set that was favourable/unfavourable to the search
  - Updated so that the test func generates multiple trees from distinct point sets for each case, and then gets the nearest each of another set of 100 target points - this should ensure that lcuky variations are evened out
  - This is not a Core_bench feature, so the raw stats are not directly comparable since we now have to divide some of the column values by the `n` multiplier
  - (see the `parsebench` script in this dir, which now generates the comparable markdown table from raw bench output)

| Key    |                             |
|--------|-----------------------------|
| `mWd`  | "minor" allocations (words) |
| `mjWd` | "major" allocations (words) |
| `Prom` | "promoted" words            |

| Name                                   | Time/run   | mWd/run   | mjWd/run  | Prom/run  | Percentage |
| -------------------------------------- | ---------: | --------: | --------: | --------: | ---------: |
| Uniform dist/pts:256 n:2500 depth:4    | 20.34us    | 15.02kw   | 20.24w    | 20.24w    | 0.18%      |
| Uniform dist/pts:256 n:2500 depth:5    | 22.21us    | 16.35kw   | 22.88w    | 22.88w    | 0.20%      |
| Uniform dist/pts:256 n:2500 depth:6    | 23.81us    | 17.29kw   | 23.89w    | 23.89w    | 0.22%      |
| Uniform dist/pts:1024 n:2500 depth:4   | 25.24us    | 18.07kw   | 27.95w    | 27.95w    | 0.23%      |
| Uniform dist/pts:1024 n:2500 depth:5   | 28.68us    | 20.04kw   | 31.87w    | 31.87w    | 0.26%      |
| Uniform dist/pts:1024 n:2500 depth:6   | 29.36us    | 21.24kw   | 34.58w    | 34.58w    | 0.27%      |
| Uniform dist/pts:65536 n:800 depth:4   | 44.27us    | 25.11kw   | 64.58w    | 64.58w    | 0.41%      |
| Uniform dist/pts:65536 n:800 depth:5   | 44.35us    | 27.13kw   | 61.26w    | 61.26w    | 0.41%      |
| Uniform dist/pts:65536 n:800 depth:6   | 50.01us    | 31.15kw   | 69.34w    | 69.34w    | 0.46%      |
| Uniform dist/pts:2097152 n:100 depth:4 | 650.46us   | 206.12kw  | 5937.55w  | 5937.55w  | 6.03%      |
| Uniform dist/pts:2097152 n:100 depth:5 | 108.77us   | 48.59kw   | 227.11w   | 227.11w   | 1.00%      |
| Uniform dist/pts:2097152 n:100 depth:6 | 59.48us    | 33.32kw   | 113.49w   | 113.49w   | 0.55%      |
| Normal dist/pts:256 n:2500 depth:4     | 37.85us    | 26.17kw   | 42.82w    | 42.82w    | 0.35%      |
| Normal dist/pts:256 n:2500 depth:5     | 42.59us    | 30.41kw   | 49.98w    | 49.98w    | 0.39%      |
| Normal dist/pts:256 n:2500 depth:6     | 47.39us    | 34.28kw   | 58.62w    | 58.62w    | 0.43%      |
| Normal dist/pts:1024 n:2500 depth:4    | 41.69us    | 28.03kw   | 57.06w    | 57.06w    | 0.38%      |
| Normal dist/pts:1024 n:2500 depth:5    | 46.72us    | 32.34kw   | 63.85w    | 63.85w    | 0.43%      |
| Normal dist/pts:1024 n:2500 depth:6    | 52.08us    | 36.40kw   | 76.26w    | 76.26w    | 0.48%      |
| Normal dist/pts:65536 n:800 depth:4    | 168.43us   | 66.18kw   | 3406.02w  | 3406.02w  | 1.56%      |
| Normal dist/pts:65536 n:800 depth:5    | 149.76us   | 61.06kw   | 3119.49w  | 3119.49w  | 1.38%      |
| Normal dist/pts:65536 n:800 depth:6    | 144.42us   | 59.92kw   | 2582.72w  | 2582.72w  | 1.33%      |
| Normal dist/pts:2097152 n:100 depth:4  | 10778.95us | 1304.35kw | 228130.7w | 228130.7w | 100.00%    |
| Normal dist/pts:2097152 n:100 depth:5  | 9298.18us  | 1115.13kw | 200595.8w | 200595.8w | 86.26%     |
| Normal dist/pts:2097152 n:100 depth:6  | 166.25us   | 59.31kw   | 1847.17w  | 1847.17w  | 1.54%      |
| Control (list cmp + sort)/pts:256      | 36.28us    | 10.01kw   | 73.48w    | 73.48w    | 0.33%      |
| Control (list cmp + sort)/pts:1024     | 186.43us   | 46.10kw   | 1303.26w  | 1303.26w  | 1.72%      |

## Interpretation

This implementation has a pre-defined depth when creating the tree. Nodes will be split down to that depth before adding any points to the leaf.

The results are still not completely stable between runs but we can see one trend - there is a definite slow-down when there are too many points and not enough depth.

We might expect to also observe a penalty for 'too much depth'. The 256 and 1024 points cases possibly show a slight effect.

In general the results were not very stable between runs. But, where depth is sufficient we see a 15-60ns timing.

#### Control

For comparison, the "control" implementation is to brute force it - measure distance for all the points in the list, sort and pop the head point.

Firstly it gave a stack overflow with 2m points, so no data there. We can also see the time is increasing faster than linear with number of points (see [2022-11-12](nearest-2022-11-12-original-implementation.md) for the 65k points case). However performance of the control is not bad with 256 points!

I ran this a few times with longer quota (10s) and it still returned consistently ~36us. I've assumed this wouldn't change with the distribution, so it's only tested with uniform points.

We would want to consistently beat that with Oktree, and TBH with 256 points that's looking marginal at the moment - Oktree is ~30% faster with uniform distribution of points, but only just matches it with a normal distribution.

Oktree depth:2 does fare better (~15us uniform, ~26us normal from a couple of manual runs), that's probably optimal for 256 in this implementation. I should probably also get round to benchmarking with the xterm 256-color palette point set, my original use case for building this lib...
