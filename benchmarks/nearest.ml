open Gg
open Owl
open Core_bench

(*
  dune build
  dune exec benchmarks/nearest.exe
  or
  _build/default/benchmarks/nearest.exe

  (if you get "Regression failed ... because the predictors were linearly
  dependent" when using a low quota try just up the quota, seems to be that)
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

let test_nearest pts targets depth =
  let trees = List.map (O.of_list depth) pts in
  Core.Staged.stage @@ (fun () ->
      List.map (fun tree ->
          List.map (fun pt -> O.nearest tree pt) targets
        ) trees
    )

let test_control n =
  let pt = target () in
  let pts = points Mat.uniform n in
  fun () ->
    List.map (fun p' -> (distance p' pt, p')) @@ pts
    |> List.sort compare
    |> List.hd
    |> snd

let main () =
  let m = 100 in
  let targets = List.init m (fun _ -> target ()) in
  let make_tests dist =
    List.map (fun (count, n) ->
        let pts = List.init n (fun _ -> points dist count) in
        Bench.Test.create_indexed
          ~name:(Printf.sprintf "pts:%i n:%i depth" count (n * m))
          ~args:[2; 3; 4; 5; 6]
        @@ test_nearest pts targets;
      ) [(256, 25); (1024, 25); (65536, 8); (2097152, 1)]
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
        (* Bench.Test.create ~name:"pts:65536" @@ test_control 65536; *)
        (* Bench.Test.create ~name:"pts:2097152 depth" @@ test_control 2097152; *)
      ];
    ])

let () = main ()