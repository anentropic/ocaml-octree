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

let compare_oktree_opt =
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
  Comparator.make cmp pp

let distance a b = Gg.V3.sub a b |> Gg.V3.norm

(* a brute force implementation as oracle *)
let nearest points p =
  if List.length points = 0 then
    raise @@ Invalid_argument "nearest oracle: points list was empty"
  else
    let result, _ =
      List.map (fun p' -> (p', distance p' p)) points
      |> List.sort (fun (_, d1) (_, d2) -> compare d1 d2)
      |> List.hd
    in
    result

let test_empty =
  test @@ fun () ->
  let root = O.empty 1 in
  let expected = Array.make 8 None in
  equal Comparator.(array compare_oktree_opt) expected root.tree.children

let test_empty_invalid =
  test @@ fun () ->
  let* depth = Sample.one_value_of [-1; 0] in
  let exc_f f = try Some (Ok (f ())) with Invalid_argument _ -> None in
  expect_raises (fun () -> O.empty depth) exc_f O.pp_root

let test_leaves =
  test @@ fun () ->
  let root = O.empty 1 in
  O.add root Gg.V3.zero;
  let leaves = O.leaves root.tree in
  equal Comparator.(list compare_ggv3) [Gg.V3.zero] leaves

let test_of_list =
  test @@ fun () ->
  let points = [Gg.V3.ox; Gg.V3.oy; Gg.V3.oz] in
  let root = O.of_list 4 points in
  let leaves = O.leaves root.tree in
  equal Comparator.(list compare_ggv3) points leaves

let test_of_list_sample_nonempty =
  test @@ fun () ->
  let* points =
    Sample.with_log
      "Sample points"
      (fun fmt points' -> pp_point_list fmt points')
      Sample.List.(non_empty @@ sample_ggv3 0. 1.)
  in
  let root = O.of_list 4 points in
  let leaves = O.leaves root.tree in
  ignore @@ equal Comparator.int (List.length leaves) (List.length points);
  equal Comparator.(list compare_ggv3) points leaves

let test_nearest1 =
  test @@ fun () ->
  let root = O.empty 1 in
  O.add root Gg.V3.zero;
  let result = O.nearest root (Gg.V3.v 0.1 0.1 0.1) in
  equal compare_ggv3 result Gg.V3.zero

let test_nearest_sample_nonempty =
  test @@ fun () ->
  let* points =
    Sample.with_log
      "Sample points"
      (fun fmt points' -> pp_point_list fmt points')
      Sample.List.(non_empty @@ sample_ggv3 0. 1.)
  in
  let* _ = 
    Sample.log_key_value "Length" (List.length points |> Int.to_string)
  in
  let root = O.of_list 4 points in
  let p = Gg.V3.v 0.2 0.5 0.7 in
  let expected = nearest points p in
  let* _ = 
    Sample.log_key_value "Expected" (Format.asprintf "%a" Gg.V3.pp expected)
  in
  let result = O.nearest root p in
  equal compare_ggv3 result expected

let test_nearest_sample_empty =
  test @@ fun () ->
  let root = O.of_list 4 [] in
  let p = Gg.V3.v 0.2 0.5 0.7 in
  let exc_f f = try Some (Ok (f ())) with Not_found -> None in
  expect_raises (fun () -> O.nearest root p) exc_f Gg.V3.pp


let tests =
  suite
    [
      ("Empty", test_empty);
      ("Empty: invalid", test_empty_invalid);
      ("Leaves", test_leaves);
      ("Of list", test_of_list);
      ("Of list: sampled points, nonempty", test_of_list_sample_nonempty);
      ("Nearest: 1", test_nearest1);
      ("Nearest: sampled points, nonempty", test_nearest_sample_nonempty);
      ("Nearest: sampled points, []", test_nearest_sample_empty);
    ]

let () = run tests
