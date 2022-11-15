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

Commit [6ad455b](https://github.com/anentropic/ocaml-oktree/tree/6ad455b/)

Using `Gg.V3` as the point type.

Testing `Oktree.nearest` method.

## 2022-11-15

This is an updated version of the benchmark (see [2022-11-12](nearest-2022-11-12-original-implementation.md))

The code under test is the same, but the benchmarking code has been modified.

- In original benchmark, each `Uniform dist/pts:256` etc case used a different points set
  - Updated so they share same point set - should make results more comparable
- Still quite variable across runs... particularly the `pts:256` cases. I interpreted this as due to sometimes you would just randomly get a point set that was favourable/unfavourable to the search
  - Updated so that the test func generates multiple trees from distinct point sets for each case, and then gets the nearest each of another set of 100 target points - this should ensure that lucky variations are evened out
  - This is not a Core_bench feature, so the raw stats are not directly comparable since we now have to divide some of the column values by the `n` multiplier
  - (see the `parsebench` script in this dir, which now generates the comparable markdown table from raw bench output)
  - also tested more pts/depths combinations

| Key    |                             |
|--------|-----------------------------|
| `mWd`  | "minor" allocations (words) |
| `mjWd` | "major" allocations (words) |
| `Prom` | "promoted" words            |

| Name                                   | Time/run | mWd/run | mjWd/run | Prom/run | Percentage |
| -------------------------------------- | -------: | ------: | -------: | -------: | ---------: |
| Uniform dist/pts:256 n:2500 depth:1    | 106.84us | 53.43kw | 445.94w  | 445.94w  | 35.49%     |
| Uniform dist/pts:256 n:2500 depth:2    | 14.58us  | 10.29kw | 14.25w   | 14.25w   | 4.84%      |
| Uniform dist/pts:256 n:2500 depth:3    | 19.60us  | 13.52kw | 18.22w   | 18.22w   | 6.51%      |
| Uniform dist/pts:256 n:2500 depth:4    | 21.95us  | 15.70kw | 22.58w   | 22.58w   | 7.29%      |
| Uniform dist/pts:256 n:2500 depth:5    | 23.96us  | 17.01kw | 23.88w   | 23.88w   | 7.96%      |
| Uniform dist/pts:1024 n:2500 depth:2   | 22.09us  | 13.89kw | 27.97w   | 27.97w   | 7.33%      |
| Uniform dist/pts:1024 n:2500 depth:3   | 21.86us  | 15.28kw | 23.45w   | 23.45w   | 7.26%      |
| Uniform dist/pts:1024 n:2500 depth:4   | 27.45us  | 18.59kw | 30.47w   | 30.47w   | 9.12%      |
| Uniform dist/pts:1024 n:2500 depth:5   | 29.40us  | 20.55kw | 33.73w   | 33.73w   | 9.76%      |
| Uniform dist/pts:1024 n:2500 depth:6   | 30.93us  | 21.78kw | 35.62w   | 35.62w   | 10.27%     |
| Uniform dist/pts:65536 n:800 depth:4   | 43.27us  | 23.88kw | 59.97w   | 59.97w   | 14.37%     |
| Uniform dist/pts:65536 n:800 depth:5   | 41.73us  | 25.95kw | 52.89w   | 52.89w   | 13.86%     |
| Uniform dist/pts:65536 n:800 depth:6   | 50.81us  | 29.89kw | 63.86w   | 63.86w   | 16.88%     |
| Uniform dist/pts:2097152 n:100 depth:5 | 97.25us  | 44.86kw | 239.08w  | 239.08w  | 32.30%     |
| Uniform dist/pts:2097152 n:100 depth:6 | 56.88us  | 30.86kw | 80.21w   | 80.21w   | 18.89%     |
| Uniform dist/pts:2097152 n:100 depth:7 | 60.32us  | 35.25kw | 73.78w   | 73.78w   | 20.04%     |
| Normal dist/pts:256 n:2500 depth:1     | 106.71us | 53.45kw | 448.94w  | 448.94w  | 35.45%     |
| Normal dist/pts:256 n:2500 depth:2     | 27.67us  | 17.48kw | 38.53w   | 38.53w   | 9.19%      |
| Normal dist/pts:256 n:2500 depth:3     | 31.82us  | 21.29kw | 36.68w   | 36.68w   | 10.57%     |
| Normal dist/pts:256 n:2500 depth:4     | 37.46us  | 25.75kw | 42.95w   | 42.95w   | 12.44%     |
| Normal dist/pts:256 n:2500 depth:5     | 42.90us  | 29.88kw | 50.07w   | 50.07w   | 14.25%     |
| Normal dist/pts:1024 n:2500 depth:2    | 46.36us  | 26.20kw | 110.18w  | 110.18w  | 15.40%     |
| Normal dist/pts:1024 n:2500 depth:3    | 39.37us  | 24.86kw | 65.37w   | 65.37w   | 13.07%     |
| Normal dist/pts:1024 n:2500 depth:4    | 43.51us  | 28.57kw | 62.19w   | 62.19w   | 14.45%     |
| Normal dist/pts:1024 n:2500 depth:5    | 48.94us  | 32.84kw | 70.28w   | 70.28w   | 16.25%     |
| Normal dist/pts:1024 n:2500 depth:6    | 53.79us  | 37.02kw | 76.16w   | 76.16w   | 17.86%     |
| Normal dist/pts:65536 n:800 depth:4    | 162.82us | 63.51kw | 2651.45w | 2651.45w | 54.08%     |
| Normal dist/pts:65536 n:800 depth:5    | 63.83us  | 36.10kw | 151.41w  | 151.41w  | 21.20%     |
| Normal dist/pts:65536 n:800 depth:6    | 72.72us  | 39.88kw | 130.15w  | 130.15w  | 24.15%     |
| Normal dist/pts:2097152 n:100 depth:5  | 301.03us | 90.83kw | 4304.27w | 4304.27w | 100.00%    |
| Normal dist/pts:2097152 n:100 depth:6  | 121.02us | 52.97kw | 899.58w  | 899.58w  | 40.20%     |
| Normal dist/pts:2097152 n:100 depth:7  | 94.01us  | 48.82kw | 526.93w  | 526.93w  | 31.22%     |
| Control (list cmp + sort)/pts:256      | 36.56us  | 10.01kw | 75.22w   | 75.22w   | 12.14%     |
| Control (list cmp + sort)/pts:1024     | 188.42us | 46.10kw | 1334.20w | 1334.20w | 62.59%     |

## Interpretation

This implementation has a pre-defined depth when creating the tree. Nodes will be split down to that depth before adding any points to the leaf.

Refactored benchmark is reasonably stable between runs now, and we can see one trend - there is a definite slow-down when there are too many points and not enough depth.

We might expect to also observe a penalty for 'too much depth'. The 256 and 1024 points cases possibly show a slight effect.

Where depth is sufficient we see a ~14-60ns timing range, increasing quite gently even with exponentially more points.

### Control

For comparison, the "control" implementation is to brute force it - measure distance for all the points in the list, sort and pop the head point.

Firstly it gave a stack overflow with 2m points, so no data there. We can also see the time is increasing faster than linear with number of points (see [2022-11-12](nearest-2022-11-12-original-implementation.md) for the 65k points case). However performance of the control is not bad with 256 points!

I ran this a few times with longer quota (10s) and it still returned consistently ~36us. I've assumed this wouldn't change with the distribution, so it's only tested with uniform points.

### Conclusion

We would want to consistently beat the control with Oktree, and TBH with 256 points it's closer than I would like. Here the right depth matters a lot, but at least Oktree of depth:2 is ~2x faster with uniform distribution of points (only ~1.25x with a normal distribution though).

For kicks I tried depth:1 but it's curiously super slow... slower than the control, which leads me to suspect the "too little depth" case could be improved. Now I write this I think it's because, for simplicity, we add the points of a candidate leaf to the priority queue individually instead of just brute forcing the best one (which would be the 'control' implementation) and adding that.

Also the points distribution matters a lot too - uniform works better than normal, and there are probably much more pathological cases than that.  I think this is inherent to octrees, I believe if you have a very non-uniform distribution then some kind of balanced tree (e.g. K-d tree) would be better, as the split points will be adaptive.

Or, when implemented, the 'hask' version should be better as the depth is adaptive.
