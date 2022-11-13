module V3 = Gg.V3

(*
My original implementation
https://github.com/anentropic/ocaml-oktree/blob/f4cd93a/lib/oktree.ml
*)

type t = {
  depth: int;
  size: float;
  root: node;
}
and node = {
  mutable children: children;
  level: int;  (* root is 0 *)
  id : int;
  origin: V3.t;
}
and children =
  | Nodes of node option array
  | Points of V3.t list
[@@deriving show]
(*
Nodes array structure:
  octant  index  bits
  x0_y0_z0: 0 | 0 0 0
  x0_y0_z1: 1 | 0 0 1
  x0_y1_z0: 2 | 0 1 0
  x0_y1_z1: 3 | 0 1 1
  x1_y0_z0: 4 | 1 0 0
  x1_y0_z1: 5 | 1 0 1
  x1_y1_z0: 6 | 1 1 0
  x1_y1_z1: 7 | 1 1 1
*)

let repeat_3 a = (a, a, a)

let octant_size root_size level = root_size /. 2. ** (Float.of_int level)

let octant_offset octant_size parent_origin index =
  let offset getter mask =
    match index land mask with  (* 0 or 1 *)
    | 0 -> getter parent_origin
    | _ -> getter parent_origin +. octant_size
  in
  V3.of_tuple ((offset V3.x 4), (offset V3.y 2), (offset V3.z 1))

(*
  Find the distance from [p] to the nearest face, edge or vertex of [octant]
  Method from: https://math.stackexchange.com/a/2133235/181250
*)
let octant_surface_distance root_size octant p =
  (*
    We want to translate our octant so that its centre lies at 0,0
    (makes the big if/else below simpler)
    We don't actually translate the octant, but rather the point [p]
    and then calculate surface distance as if octant was translated.
    TODO: scaled octant offsets could be pre-calculated, the scalings
    are different for each octant but they are static. However...
    This answer https://stackoverflow.com/a/48330314/202168 suggests that
    with large numbers of points (10k) it's faster *not* to precalculate
    because reducing memory size of nodes improves cpu cache usage.
    It also strongly recommends not storing points in the leaves, and
    perhaps not nodes within ndoes at all, instead these should be indexes
    into a flat top-level data structure, again for better cpu cache usage.
  *)
  let octsize = octant_size root_size octant.level in
  let s = octsize /. 2. in
  (* Format.printf "| s: %f\n" s; *)
  let oct_centre_offset = V3.add octant.origin (V3.of_tuple (repeat_3 s)) in
  (* root origin is a distance from 0,0, octant.origin is relative to root origin *)
  let offset = V3.sub (V3.of_tuple (0., 0., 0.)) oct_centre_offset in
  let p' = V3.add p offset in
  (*
    For this calculation the point [p] has one of 27 possible positions
    in relation to the octant, which correspond to the 26 adjacent octants
    plus potentially being within the target octant.
    26 adjacent = 6 face:face + 8 vertex:vertex + 12 edge:edge
    By taking |abs| values these are collapsed down to the 7+1 cases below,
    finding the distance to nearest face, vertex or edge as appropriate.
    (I think these are Euclidean distance too)
  *)
  let x, y, z = p' |> V3.map abs_float |> V3.to_tuple in
  if x <= s then
    if y <= s then
      (* without the max this would give negative distance in case where
          p is in octant - that would actually still work with our pq *)
      begin
        (* face *)
        max 0. (z -. s)  (* 0 if p in octant, all other cases return > 0 *)
      end
    else
    if z <= s then
      begin
        (* face *)
        y -. s
      end
    else
      begin
        (* edge *)
        sqrt ((y -. s) ** 2. +. (z -. s) ** 2.)
      end
  else
  if y <= s then
    if z <= s then
      begin
        (* face *)
        x -. s
      end
    else
      begin
        (* edge *)
        sqrt ((x -. s) ** 2. +. (z -. s) ** 2.)
      end
  else
  if z <= s then
    begin
      (* edge *)
      sqrt ((x -. s) ** 2. +. (y -. s) ** 2.)
    end
  else
    begin
      (* vertex *)
      sqrt ((x -. s) ** 2. +. (y -. s) ** 2. +. (z -. s) ** 2.)
    end
