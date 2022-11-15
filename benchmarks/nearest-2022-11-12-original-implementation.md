# Benchmark

OCaml 4.14  
Macbook Air M1 16GB macOS 13.0

Made using https://github.com/janestreet/core_bench

```shell
dune build
_build/default/benchmarks/nearest.exe -quota 3
```

### "Original implementation"

Commit [d1f6ca1](https://github.com/anentropic/ocaml-oktree/tree/d1f6ca1/)

Using `Gg.V3` as the point type.

Testing `Oktree.nearest` method.

## 2022-11-12

(`mWd/Run`, `mjWd/Run` and `Prom/Run` are memory-allocation stats)

| Name                             | Time/Run |  mWd/Run |  mjWd/Run |  Prom/Run | Percentage |
|----------------------------------|----------|----------|-----------|-----------|------------|
| Uniform dist/pts:256 depth:4     |  16.71us |  13.25kw |    13.65w |    13.65w |      2.74% |
| Uniform dist/pts:256 depth:5     |  21.55us |  16.86kw |    19.33w |    19.33w |      3.53% |
| Uniform dist/pts:256 depth:6     |  38.01us |  28.65kw |    48.45w |    48.45w |      6.23% |
| Uniform dist/pts:1024 depth:4    |  15.08us |  11.91kw |    11.53w |    11.53w |      2.47% |
| Uniform dist/pts:1024 depth:5    |  19.77us |  15.95kw |    17.41w |    17.41w |      3.24% |
| Uniform dist/pts:1024 depth:6    |  22.02us |  16.66kw |    20.69w |    20.69w |      3.61% |
| Uniform dist/pts:65536 depth:4   |  30.12us |  19.76kw |    34.56w |    34.56w |      4.93% |
| Uniform dist/pts:65536 depth:5   |  36.29us |  25.77kw |    46.17w |    46.17w |      5.94% |
| Uniform dist/pts:65536 depth:6   |  50.14us |  32.01kw |    62.68w |    62.68w |      8.21% |
| Uniform dist/pts:2097152 depth:4 | 318.15us | 128.92kw | 2_047.52w | 2_047.52w |     52.11% |
| Uniform dist/pts:2097152 depth:5 |  51.95us |  30.31kw |    82.28w |    82.28w |      8.51% |
| Uniform dist/pts:2097152 depth:6 |  95.17us |  58.43kw |   227.50w |   227.50w |     15.59% |
| Normal dist/pts:256 depth:4      |  14.19us |  11.64kw |     9.94w |     9.94w |      2.32% |
| Normal dist/pts:256 depth:5      |  19.78us |  15.19kw |    15.92w |    15.92w |      3.24% |
| Normal dist/pts:256 depth:6      |  62.39us |  45.82kw |    68.13w |    68.13w |     10.22% |
| Normal dist/pts:1024 depth:4     |  24.72us |  19.00kw |    22.90w |    22.90w |      4.05% |
| Normal dist/pts:1024 depth:5     |  39.80us |  29.89kw |    44.99w |    44.99w |      6.52% |
| Normal dist/pts:1024 depth:6     |  18.82us |  14.89kw |    14.80w |    14.80w |      3.08% |
| Normal dist/pts:65536 depth:4    | 610.50us | 249.63kw | 6_420.60w | 6_420.60w |    100.00% |
| Normal dist/pts:65536 depth:5    |  30.24us |  21.98kw |    32.86w |    32.86w |      4.95% |
| Normal dist/pts:65536 depth:6    |  22.01us |  17.11kw |    21.85w |    21.85w |      3.61% |
| Normal dist/pts:2097152 depth:4  | 313.25us | 129.22kw | 2_042.67w | 2_042.67w |     51.31% |
| Normal dist/pts:2097152 depth:5  | 129.15us |  64.80kw |   404.11w |   404.11w |     21.15% |
| Normal dist/pts:2097152 depth:6  |  74.18us |  46.38kw |   148.18w |   148.18w |     12.15% |
| Control (list cmp + sort)/pts:256 depth   |     36.80us |    10.01kw |        68.69w |        68.69w |      0.10% |
| Control (list cmp + sort)/pts:1024 depth  |    201.69us |    46.10kw |     1_306.24w |     1_306.24w |      0.55% |
| Control (list cmp + sort)/pts:65536 depth | 36_525.54us | 4_128.79kw | 1_150_890.16w | 1_150_890.16w |    100.00% |

## Interpretation

This implementation has a pre-defined depth when creating the tree. Nodes will be split down to that depth before adding any points to the leaf.

The results were not super stable between runs but we can see one trend - there is a slow-down when there are too many points and not enough depth.

We might expect to also observe a penalty for 'too much depth'. The 256 points cases possibly show it, goes away with more points though (even with higher depths than tested here).

In general the results were not very stable between runs. But, where depth is sufficient we see a 15-60ns timing.

#### Control

For comparison, the "control" implementation is to brute force it - measure distance for all the points in the list, sort and pop the head point.

Firstly it gave a stack overflow with 2m points, so no data there. We can also see the time is increasing faster than linear with number of points. However performance is pretty good with 256 points!

I ran this a few times with longer quota (10s) and it returned consistently ~36us. I've assumed this wouldn't change with the distribution, so it's only tested with uniform points.

We would want to consistently beat that with Oktree, and TBH with 256 points that's looking marginal at the moment.
