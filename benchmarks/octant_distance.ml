open Owl
open Core_bench

(*
  dune build
  dune exec benchmarks/benchmark.exe
  or
  _build/default/benchmarks/octant_distance.exe
*)

let points dist n =
  let values = dist 3 n in
  List.init n (fun i ->
      match Mat.col values i |> Mat.to_array with
      | [|x; y; z|] -> Gg.V3.v x y z
      | _ -> raise @@ Invalid_argument "Wrong shape matrix"
    )

let point () = List.hd @@ points Mat.uniform 1

let original' =
  let open Octd_original in
  let nodes =
    List.init 8 (fun i ->
        {
          children = Nodes (Array.make 8 None);
          level = 1;
          id = 0;
          origin = octant_offset 0.5 V3.zero i;
        }
      )
  in
  fun pt ->
    List.map (fun node -> octant_surface_distance 1. node pt) nodes

let hask' =
  let open Octd_hask in
  let node = {
    split = V3.v 0.5 0.5 0.5;
    nwu = Leaf { points = [] };
    nwd = Leaf { points = [] };
    neu = Leaf { points = [] };
    ned = Leaf { points = [] };
    swu = Leaf { points = [] };
    swd = Leaf { points = [] };
    seu = Leaf { points = [] };
    sed = Leaf { points = [] };
  }
  in
  fun pt ->
    octantDistances @@ (V3.sub pt node.split)

type float_list = float list [@@deriving show]

(* TESTS *)

let test_original pt = fun () -> original' pt
let test_hask pt = fun () -> hask' pt

let main () =
  let pt = point () in
  Command_unix.run (Bench.make_command [
      Bench.Test.create ~name:"original" @@ test_original pt;
      Bench.Test.create ~name:"'hask'" @@ test_hask pt;
    ]);
  let original_result = List.sort compare @@ original' pt in
  let hask_result =
    List.map (fun (_, d) -> d) (hask' pt)
    |> List.sort compare
  in
  (* sometimes the values 'print' equal but don't compare equal
     so we need to use 'tolerant' equality... *)
  let cmp_float = Gg.Float.(equal_tol ~eps:0.000001) in
  if List.equal cmp_float original_result hask_result
  then print_endline "Results agree 👍"
  else begin
    print_endline "Results disagree 😞";
    Format.printf "Original result: %a\n" pp_float_list original_result;
    Format.printf "'Hask' result  : %a\n" pp_float_list hask_result;
    List.iter2 (fun a b ->
        if (cmp_float a b) = false then
          Format.printf "OG: %f Hask: %f diff: %f\n" a b (a -. b);        
      ) original_result hask_result;
  end

let () = main ()
