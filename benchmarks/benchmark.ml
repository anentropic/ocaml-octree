open Gg
open Owl
open Core_bench

(*
  dune build
  dune exec benchmarks/benchmark.exe
  or
  _build/default/benchmarks/benchmark.exe
*)

module O = Oktree.Make (V3)

let points dist n =
  let values = dist 3 n in
  List.init n (fun i ->
      match Mat.col values i |> Mat.to_array with
      | [|x; y; z|] -> V3.v x y z
      | _ -> raise @@ Invalid_argument "Wrong shape matrix"
    )

let target () = List.hd @@ points Mat.uniform 1

let distance a b = V3.sub a b |> V3.norm

(* TESTS *)

(* points in a 'uniform' distribution
   ...are completely random, although can look 'clumpy' to the eye*)
let test_uniform n depth =
  let tree = O.of_list depth (points Mat.uniform n) in
  let pt = target () in
  Core.Staged.stage @@ fun () -> O.nearest tree pt

(* points in a 'normal' distribution
   ...will be denser in the centre / have more outliers*)
let test_normal n depth =
  let tree = O.of_list depth (points Mat.gaussian n) in
  let pt = target () in
  Core.Staged.stage @@ fun () -> O.nearest tree pt

let test_control n =
  let pt = target () in
  let pts = points Mat.uniform n in
  fun () ->
    List.map (fun p' -> (distance p' pt, p')) @@ pts
    |> List.sort compare
    |> List.hd
    |> snd

let main () =
  ignore @@ Command_unix.run (Bench.make_command [
      Bench.Test.create_group ~name:"Uniform dist" [
        Bench.Test.create_indexed ~name:"pts:256 depth" ~args:[4; 5; 6;] @@ test_uniform 256;
        Bench.Test.create_indexed ~name:"pts:1024 depth" ~args:[4; 5; 6;] @@ test_uniform 1024;
        Bench.Test.create_indexed ~name:"pts:65536 depth" ~args:[4; 5; 6;] @@ test_uniform 65536;
        Bench.Test.create_indexed ~name:"pts:2097152 depth" ~args:[4; 5; 6;] @@ test_uniform 2097152;
      ];
      Bench.Test.create_group ~name:"Normal dist" [
        Bench.Test.create_indexed ~name:"pts:256 depth" ~args:[4; 5; 6;] @@ test_normal 256;
        Bench.Test.create_indexed ~name:"pts:1024 depth" ~args:[4; 5; 6;] @@ test_normal 1024;
        Bench.Test.create_indexed ~name:"pts:65536 depth" ~args:[4; 5; 6;] @@ test_normal 65536;
        Bench.Test.create_indexed ~name:"pts:2097152 depth" ~args:[4; 5; 6;] @@ test_uniform 2097152;
      ];
      Bench.Test.create_group ~name:"Control (list cmp + sort)" [
        Bench.Test.create ~name:"pts:256 depth" @@ test_control 256;
        Bench.Test.create ~name:"pts:1024 depth" @@ test_control 1024;
        Bench.Test.create ~name:"pts:65536 depth" @@ test_control 65536;
        (* Bench.Test.create ~name:"pts:2097152 depth" @@ test_control 2097152; *)
      ];
    ])

let () = main ()
