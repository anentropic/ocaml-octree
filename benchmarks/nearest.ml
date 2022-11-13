open Gg
open Owl
open Core_bench

(*
  dune build
  dune exec benchmarks/nearest.exe
  or
  _build/default/benchmarks/nearest.exe
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

let test_nearest pts pt =
  let root = O.of_list pts in
  fun () -> O.nearest root.tree pt

let test_control n =
  let pt = target () in
  let pts = points Mat.uniform n in
  fun () ->
    List.map (fun p' -> (distance p' pt, p')) @@ pts
    |> List.sort compare
    |> List.hd
    |> snd

let main () =
  let pt = target () in
  let make_tests dist =
    List.map (fun i ->
        let pts = points dist i in
        Bench.Test.create ~name:(Printf.sprintf "pts:%i" i) @@ test_nearest pts pt;
      ) [256; 1024; 65536; 2097152]
  in
  (*
    - points in a 'uniform' distribution are completely random, although can
     look 'clumpy' to the eye
    - 'gaussian' or 'normal' distribution is denser in the middle of the range
      and has more sparse outliers
  *)
  ignore @@ Command_unix.run (Bench.make_command [
      Bench.Test.create_group ~name:"Uniform dist" @@ make_tests Mat.uniform;
      Bench.Test.create_group ~name:"Normal dist" @@ make_tests Mat.gaussian;
      Bench.Test.create_group ~name:"Control (list cmp + sort)" [
        Bench.Test.create ~name:"pts:256" @@ test_control 256;
        Bench.Test.create ~name:"pts:1024" @@ test_control 1024;
        Bench.Test.create ~name:"pts:65536" @@ test_control 65536;
        (* Bench.Test.create ~name:"pts:2097152 depth" @@ test_control 2097152; *)
      ];
    ])

let () = main ()
