open Bechamel
open Gg
open Owl

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

let product2 a b =
  List.concat_map (fun x ->
      List.map (fun y -> x, y) b
    ) a

(* tests *)

let test_uniform depth n =
  let tree = O.of_list depth (points Mat.uniform n) in
  let pt = target () in
  let name = Printf.sprintf "uniform, depth: %i, %i pts" depth n in
  Test.make ~name (Staged.stage @@ fun () -> O.nearest tree pt)

let test_normal depth n =
  let tree = O.of_list depth (points Mat.gaussian n) in
  let pt = target () in
  let name = Printf.sprintf "normal, depth: %i, %i pts" depth n in
  Test.make ~name (Staged.stage @@ fun () -> O.nearest tree pt)


let test =
  let args = product2 [4; 8; 12;] [256; 1024; 65536;] in
  let tests maker =
    List.map (fun (depth, count) -> maker depth count) args
  in
  (* the full test suite
     timings seem to vary wildly between runs though 😞 *)
  Test.make_grouped ~name:"Oktree" ~fmt:"%s: %s" [
    Test.make_grouped ~name:"Uniform dist" ~fmt:"%s: (%s)" @@ tests test_uniform;
    Test.make_grouped ~name:"Normal dist" ~fmt:"%s: (%s)" @@ tests test_normal;
  ]
(* seem to get different GC behaviour if we just run a couple of tests
   ...it shows 0 mjw/run for the full suite 🤷‍♂️ *)
(* Test.make_grouped ~name:"Oktree" ~fmt:"%s: %s" [
   test_uniform 4 256;
   test_uniform 4 1024;
   ] *)

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Toolkit.Instance.[ minor_allocated; major_allocated; monotonic_clock ]
  in
  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 0.5) ~kde:(Some 1000) ~stabilize:true ()
  in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let () =
  List.iter
    (fun v -> Bechamel_notty.Unit.add v (Measure.unit v))
    Toolkit.Instance.[ minor_allocated; major_allocated; monotonic_clock ]

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let () =
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  let results, _ = benchmark () in
  print_endline "";
  img (window, results) |> eol |> output_image
