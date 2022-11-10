open Popper
open Sample.Syntax

module O = Oktree.Make (Gg.V3)

type point_list = Gg.V3.t list [@@deriving show]

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

let test_empty =
  test @@ fun () ->
  let* depth = Sample.Int.range 2 16 in
  let tree = O.empty depth in
  let expected : O.t =
    {
      depth;
      size = 1.;
      root = {
        children = Nodes (Array.make 8 None);
        level = 0;
        id = 0;
        origin = Gg.V3.of_tuple (0., 0., 0.);
      }
    }
  in
  equal compare_oktree expected tree

let test_empty_depth_1 =
  test @@ fun () ->
  let depth = 1 in
  let tree = O.empty depth in
  let expected : O.t =
    {
      depth;
      size = 1.;
      root = {
        children = Points [];
        level = 0;
        id = 0;
        origin = Gg.V3.of_tuple (0., 0., 0.);
      }
    }
  in
  equal compare_oktree expected tree

let test_empty_invalid =
  test @@ fun () ->
  (* tree depth must be in range 1..20 inclusive *)
  let* depth = Sample.one_value_of [-1; 0; 21;] in
  let exc_f f = try Some (Ok (f ())) with Invalid_argument _ -> None in
  expect_raises (fun () -> O.empty depth) exc_f O.pp

let test_points =
  test @@ fun () ->
  let tree = O.empty 1 in
  O.add tree Gg.V3.zero;
  let points = O.points tree.root in
  equal Comparator.(list compare_ggv3) [Gg.V3.zero] points

let test_of_list =
  test @@ fun () ->
  let points = [Gg.V3.ox; Gg.V3.oy; Gg.V3.oz] |> sort_ggv3_list in
  let tree = O.of_list 4 points in
  let points = O.points tree.root |> sort_ggv3_list in
  equal Comparator.(list compare_ggv3) points points

let test_of_list_sample_nonempty =
  test @@ fun () ->
  let* points =
    Sample.with_log
      "Sample points"
      (fun fmt points' -> pp_point_list fmt points')
      Sample.List.(non_empty @@ sample_ggv3 0. 1.)
  in
  let tree = O.of_list 4 points in
  let points = O.points tree.root |> sort_ggv3_list in
  ignore @@ equal Comparator.int (List.length points) (List.length points);
  equal Comparator.(list compare_ggv3) (sort_ggv3_list points) points

let test_nearest1 =
  test @@ fun () ->
  let points = [Gg.V3.zero; Gg.V3.v 0. 0.251 0.; Gg.V3.v 0. 0.23 0.; Gg.V3.v 0.2 0.1 0.2] in
  let tree = O.of_list 4 points in
  let target = Gg.V3.v 0.24 0.24 0.24 in
  let result = O.nearest tree target in
  let* expected = nearest points target in
  equal compare_ggv3 result expected

let test_nearest2 =
  test @@ fun () ->
  let points = [Gg.V3.v 0.22211 0.310896 0.380155; Gg.V3.v 0. 0. 0.; Gg.V3.v 0.154595 0.444363 0.909263] in
  let tree = O.of_list 4 points in
  let target = Gg.V3.v 0.3333 0.41 0.6667 in
  let* expected = nearest points target in
  let result = O.nearest tree target in
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

let test_nearest3 =
  test @@ fun () ->
  let points = [Gg.V3.v 0. 0.849467 0.16977; Gg.V3.v 0. 0. 0.175422] in
  let tree = O.of_list 4 points in
  let target = Gg.V3.v 0.3333 0.41 0.6667 in
  let* expected = nearest points target in
  let result = O.nearest tree target in
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

let test_nearest4 =
  test @@ fun () ->
  let points = [Gg.V3.v 0.0408104 0.120397 0.712801; Gg.V3.v 0.754196 0.425501 0.700406] in
  let tree = O.of_list 4 points in
  let target = Gg.V3.v 0.3333 0.41 0.6667 in
  let* expected = nearest points target in
  let result = O.nearest tree target in
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

let test_nearest_sample_nonempty =
  test @@ fun () ->
  let* points =
    Sample.with_log
      "Sample points"
      pp_point_list
      Sample.List.(non_empty @@ sample_ggv3 0. 1.)
  in
  (* let* target =
     Sample.with_log
      "Target point"
      Gg.V3.pp
      (sample_ggv3 0. 1.)
     in *)
  let target = Gg.V3.v 0.3333 0.41 0.6667 in
  let* _ = 
    Sample.log_key_value "Length" (List.length points |> Int.to_string)
  in
  let tree = O.of_list 4 points in
  let* expected = nearest points target in
  let result = O.nearest tree target in
  let* _ = 
    Sample.log_key_value "Expected" (Format.asprintf "%a" Gg.V3.pp expected)
  in
  let* _ = 
    Sample.log_key_value "Expected distance" @@ Float.to_string (distance target expected)
  in
  let* _ = 
    Sample.log_key_value "Result distance" @@ Float.to_string (distance target result)
  in
  (* let* _ = 
     Sample.log_key_value "Tree" @@ (Format.asprintf "%a" O.pp tree)
     in *)
  (* let* _ = 
     Sample.log_key_value "Stats" @@ (Format.asprintf "%a" O.pp_tree_stats @@ O.stats tree)
     in *)
  equal compare_ggv3 result expected

let test_nearest_sample_empty =
  test @@ fun () ->
  let root = O.of_list 4 [] in
  let p = Gg.V3.v 0.2 0.5 0.7 in
  let exc_f f = try Some (Ok (f ())) with Not_found -> None in
  expect_raises (fun () -> O.nearest root p) exc_f Gg.V3.pp

let empty_suite = 
  suite
    [
      ("max-depth 1", test_empty_depth_1);
      ("max-depth 2..16", test_empty);
      ("invalid", test_empty_invalid);
    ]

let of_list_suite = 
  suite
    [
      ("3 static points", test_of_list);
      ("sampled points, nonempty", test_of_list_sample_nonempty);
    ]

let nearest_suite = 
  suite
    [
      ("1. static points, depth 4", test_nearest1);
      ("2. static points, depth 4", test_nearest2);
      ("3. static points, depth 4", test_nearest3);
      ("4. static points, depth 4", test_nearest4);
      ("sampled points, nonempty, depth 4", test_nearest_sample_nonempty);
      ("sampled points, []", test_nearest_sample_empty);
    ]

let tests =
  suite
    [
      ("empty", empty_suite);
      ("points", test_points);
      ("of_list", of_list_suite);
      ("nearest", nearest_suite);
    ]

let () = run tests
