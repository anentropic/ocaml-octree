open Popper
open Sample.Syntax

module O = Oktree.Make (Gg.V3)

let pp_vec3 fmt p =
  let x, y, z = Gg.V3.to_tuple p in
  Format.fprintf fmt "(%f, %f, %f)" x y z

let pp_point_list fmt =
  let pp_sep fmt' () = Format.pp_print_string fmt' "; " in
  let ppl fmt' = Format.pp_print_list ~pp_sep pp_vec3 fmt' in
  Format.fprintf fmt "[%a]" ppl


let strict_float_range low high =
  let rec sample () =
    let* x = Sample.Float.range low high in
    if x < low || x > high then
      sample ()
    else
      Sample.return x
  in
  sample ()

let expect_raises f exc_f pp =
  let result_opt =
    try exc_f f
    with e -> Some (Error e)
  in
  match result_opt with
  | Some (Ok a) ->
    fail @@ Format.asprintf "Unexpected result: %a" pp a
  | Some (Error e) ->
    fail @@ Printf.sprintf "Unexpected error: %s" (Printexc.to_string e) 
  | None -> pass  (* the correct result *)


let sample_ggv3 low high =
  let* x = strict_float_range low high in
  let* y = strict_float_range low high in
  let* z = strict_float_range low high in
  Sample.return (Gg.V3.v x y z)

let compare_ggv3 =
  Comparator.make Gg.V3.compare Gg.V3.pp

let sort_ggv3_list = List.sort Gg.V3.compare

(* let compare_ggv3_opt =
   let cmp a b =
    match (a, b) with
    | (Some a', Some b') -> Gg.V3.compare a' b'
    | (Some _, None) -> 1
    | (None, Some _) -> -1
    | (None, None) -> 0
   in
   let pp fmt p = 
    match p with
    | Some p -> Format.fprintf fmt "Some %a" Gg.V3.pp p
    | None -> ignore @@ Format.fprintf fmt (format_of_string "None")
   in
   Comparator.make cmp pp *)

(* let compare_oktree_opt =
   let cmp a b =
    match (a, b) with
    | (Some a', Some b') -> compare a' b'
    | (Some _, None) -> 1
    | (None, Some _) -> -1
    | (None, None) -> 0
   in
   let pp fmt p = 
    match p with
    | Some _ -> Format.fprintf fmt (format_of_string "Some")
    | None -> Format.fprintf fmt (format_of_string "None")
   in
   Comparator.make cmp pp *)

let compare_oktree = Comparator.make compare O.pp
let compare_oktree_node = Comparator.make compare O.pp_node

let distance a b = Gg.V3.sub a b |> Gg.V3.norm

type point_distance_list = (float * Gg.V3.t) list [@@deriving show]

(* a brute force implementation as oracle *)
let nearest points p =
  if List.length points = 0 then
    raise @@ Invalid_argument "nearest oracle: points list was empty"
  else
    let sorted =
      List.map (fun p' -> (distance p' p, p')) points
      |> List.sort compare
    in
    let* _ = 
      Sample.log_key_value "Oracle" @@ (Format.asprintf "%a" pp_point_distance_list sorted)
    in
    let _, result = List.hd sorted in
    Sample.return result

let from_tuples l = List.map (fun (x,y,z) -> Gg.V3.v x y z) l

(* TESTS *)

let test_of_list =
  test @@ fun () ->
  let points = [Gg.V3.ox; Gg.V3.oy; Gg.V3.oz] |> sort_ggv3_list in
  let root = O.of_list points in
  let points = O.to_list root.tree |> sort_ggv3_list in
  equal Comparator.(list compare_ggv3) points points

let test_of_list_sample_nonempty =
  test @@ fun () ->
  let* points =
    Sample.with_log
      "Sample points"
      (fun fmt points' -> pp_point_list fmt points')
      Sample.List.(non_empty @@ sample_ggv3 0. 1.)
  in
  let root = O.of_list points in
  let points = O.to_list root.tree |> sort_ggv3_list in
  ignore @@ equal Comparator.int (List.length points) (List.length points);
  equal Comparator.(list compare_ggv3) (sort_ggv3_list points) points

let test_nearest_handpicked_failures =
  let make points target =
    test @@ fun () ->
    let root = O.of_list points in
    let* expected = nearest points target in
    let result = O.nearest root.tree target in
    let* _ = 
      Sample.log_key_value "Expected" (Format.asprintf "%a" Gg.V3.pp expected)
    in
    let* _ = 
      Sample.log_key_value "Expected distance" @@ Float.to_string (distance target expected)
    in
    let* _ = 
      Sample.log_key_value "Result distance" @@ Float.to_string (distance target result)
    in
    equal compare_ggv3 result expected
  in
  let args =
    [
      (* ([Gg.V3.zero; Gg.V3.v 0. 0.251 0.; Gg.V3.v 0. 0.23 0.; Gg.V3.v 0.2 0.1 0.2],
         Gg.V3.v 0.24 0.24 0.24); *)
      (* ([Gg.V3.v 0.22211 0.310896 0.380155; Gg.V3.v 0. 0. 0.; Gg.V3.v 0.154595 0.444363 0.909263],
         Gg.V3.v 0.3333 0.41 0.6667); *)
      (* ([Gg.V3.v 0. 0.849467 0.16977; Gg.V3.v 0. 0. 0.175422],
         Gg.V3.v 0.3333 0.41 0.6667); *)
      (* ([Gg.V3.v 0.0408104 0.120397 0.712801; Gg.V3.v 0.754196 0.425501 0.700406],
         Gg.V3.v 0.3333 0.41 0.6667); *)
      (from_tuples [(0.000000, 0.135509, 0.558065); (0.000000, 0.000000, 0.251862); (0.000000, 0.000000, 0.309942); (0.000000, 0.818889, 0.000000); (0.558965, 0.114604, 0.000000); (0.000000, 0.000000, 0.000000); (0.000000, 0.000000, 0.297470); (0.000000, 0.000000, 0.449710); (0.000000, 0.000000, 0.302328); (0.497573, 0.000000, 0.000000); (0.000000, 0.000000, 0.449739); (0.000000, 0.000000, 0.302581); (0.000000, 0.933309, 0.000000); (0.000000, 0.000000, 0.000000); (0.000000, 0.000000, 0.321395); (0.000000, 0.000000, 0.407899); (0.032235, 0.087385, 0.615754); (0.000000, 0.373305, 0.000000); (0.000000, 0.000000, 0.432156); (0.000000, 0.000000, 0.000000); (0.000000, 0.000000, 0.247959); (0.000000, 0.000000, 0.361187); (0.000000, 0.000000, 0.000000); (0.000000, 0.000000, 0.483175); (0.000000, 0.000000, 0.367874); (0.000000, 0.000000, 0.445183); (0.000000, 0.000000, 0.000000); (0.000000, 0.000000, 0.306433)],
       Gg.V3.v 0.3333 0.41 0.6667);
    ]
  in
  suite @@ List.mapi (fun i (points, target) ->
      (Printf.sprintf "%i" i), (make points target)
    ) args

let test_nearest_sample_nonempty =
  let configs = [Config.num_samples 5000; Config.seed [1]] in
  test ~config:(Config.all configs) @@ fun () ->
  let* points =
    Sample.with_log
      "Sample points"
      pp_point_list
      Sample.List.(non_empty @@ sample_ggv3 0. 1.)
  in
  let target = Gg.V3.v 0.3333 0.41 0.6667 in
  let* _ = 
    Sample.log_key_value "Length" (List.length points |> Int.to_string)
  in
  let root = O.of_list points in
  let* expected = nearest points target in
  let result = O.nearest root.tree target in
  let* _ = 
    Sample.log_key_value "Expected" (Format.asprintf "%a" Gg.V3.pp expected)
  in
  let* _ = 
    Sample.log_key_value "Expected distance" @@ Float.to_string (distance target expected)
  in
  let* _ = 
    Sample.log_key_value "Result distance" @@ Float.to_string (distance target result)
  in
  equal compare_ggv3 result expected

let test_nearest_sample_empty =
  test @@ fun () ->
  let root = O.of_list [] in
  let p = Gg.V3.v 0.2 0.5 0.7 in
  let exc_f f = try Some (Ok (f ())) with Not_found -> None in
  expect_raises (fun () -> O.nearest root.tree p) exc_f Gg.V3.pp


(* RUNNER *)

let of_list_suite = 
  suite
    [
      ("3 static points", test_of_list);
      ("sampled points, nonempty", test_of_list_sample_nonempty);
    ]

let nearest_suite = 
  suite
    [
      ("handpicked failures", test_nearest_handpicked_failures);
      (* ("sampled points, nonempty", test_nearest_sample_nonempty); *)
      (* ("sampled points, []", test_nearest_sample_empty); *)
    ]

let tests =
  suite
    [
      (* ("of_list", of_list_suite); *)
      ("nearest", nearest_suite);
    ]

let () = run tests
