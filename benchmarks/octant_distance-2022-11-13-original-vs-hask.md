# Benchmark

OCaml 4.14  
Macbook Air M1 16GB macOS 13.0

Made using https://github.com/janestreet/core_bench

```shell
dune build
_build/default/benchmarks/octant_distance.exe
```

## "Original" implementation

The octant surface distance algo I used comes from here: https://math.stackexchange.com/a/2133235/181250

The original version required both a translation and a scaling, I eliminated the scaling.

But in both cases it requires that we know the size (length of side) of our octants.

See: https://github.com/anentropic/ocaml-oktree/blob/9397913/benchmarks/octd_original.ml

Using `Gg.V3` as the point type.

Testing `octant_surface_distance` function against the 8 child nodes in an octant.

## "Hask" implementation

I found this Haskell octree implementation: https://github.com/BioHaskell/octree

One nice thing I saw in their version was you don't have to know the octant size. This means you don't have to specify the bounding-box size of root of the tree up-front. So instead of an origin and a size, the octants only need a centre point.

So I converted their code to OCaml with a view to borrowing it. It was pretty straightforward to do a literal translation.

See: https://github.com/anentropic/ocaml-oktree/blob/9397913/benchmarks/octd_hask.ml

Using `Gg.V3` as the point type.

Testing `octantDistances` function, which returns distance to each child octant.

### 2022-11-13

(`mWd/Run`, `mjWd/Run` and `Prom/Run` are GC stats)

| Name     | Time/Run | mWd/Run | Percentage |
|----------|----------|---------|------------|
| original | 397.96ns | 412.00w |    100.00% |
| 'hask'   | 217.99ns | 230.00w |     54.78% |

Results agree 👍

## Interpretation

(initially I had coded a duff benchmark, where the versions under test were doing different things... "Results agree" check was added to ensure that is not happening!)

This result was quite stable - the 'hask' emplementation is approx 2x faster than my original one.
