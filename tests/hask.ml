module V3 = Gg.V3

(*
The relevant parts of
https://github.com/BioHaskell/octree/blob/master/Data/Octree/Internal.hs

I wanted to understand how they get around needing to know octant size
in the octant surface distance calculation
*)

type octree =
  | Node of node
  | Leaf of { points : V3.t list }
[@@deriving show, iter, map, fold]
and node = {
  split : V3.t;
  nwu : octree;
  nwd : octree;
  neu : octree;
  ned : octree;
  swu : octree;
  swd : octree;
  seu : octree;
  sed : octree;
}
[@@deriving show]

(* an octant identifier *)
type odir = SWD | SED | NWD | NED | SWU | SEU | NWU | NEU
[@@deriving eq, ord, enum, show, iter]


(* make an odir for bool flags *)
let joinStep (cx, cy, cz) =
  odir_of_enum ((Bool.to_int cx) + 2 * (Bool.to_int cy) + 4 * (Bool.to_int cz))
  |> Option.get

(* odir to bool flags (i.e. which axis-bits are set) *)
let splitStep step =
  let a = odir_to_enum step in
  ((a land 1) == 1, (a land 2) == 2, (a land 4) == 4)

let octantDistance' dp = function
  (* pt is in this octant *)
  | NEU -> 0.0
  (* adjacent by plane *)
  | NWU -> V3.x dp
  | SEU -> V3.y dp
  | NED -> V3.z dp
  (* adjacent by edge *)
  | SWU -> sqrt ( V3.x dp ** 2. +. V3.y dp ** 2.)
  | SED -> sqrt ( V3.y dp ** 2. +. V3.z dp ** 2.)
  | NWD -> sqrt ( V3.x dp ** 2. +. V3.z dp ** 2.)
  (* adjacent by vertex *)
  | SWD -> V3.norm dp

let allOctants =
  List.init max_odir (fun i -> odir_of_enum i |> Option.get)


(*
  NOTE: the returned odir no longer represents an octant
  per se but one of 8 derived possibilities
  ...this is a confusing overloading!
  ...but it saves creating another 8 member variant and
    identical join/split functions 

  we can see in [octantDistance'] above what they mean
  i.e. NEU means pt is "in the tested octant"

  The boolean checks expand out like:
    true != (not false) -> false
    false != (not false) -> true
    true != (not true) -> true
    false != (not true) -> false

  so to get NEU we need:
    x >= 0 and u = true  -> confirmed in the +x octant
    x < 0  and u = false -> confirmed in the -x octant
    etc

  and then when fewer bits are set we get the other options
*)
let toggle dp odir =
  let (u, v, w) = splitStep odir in
  joinStep (
    (V3.x dp >= 0.) <> (not u),
    (V3.y dp >= 0.) <> (not v),
    (V3.z dp >= 0.) <> (not w)
  ) 

(*
  where [dp] is difference between pt and octant centre
  ...which is like translating octant centre (with pt) to 0,0,0 origin
  and [odir] is the child octant id
*)
let octantDistance dp odir =
  octantDistance' (V3.map abs_float dp) (toggle dp odir)

let octantDistances dp =
  List.map (fun o -> (o, octantDistance dp o)) allOctants


(* didn't bother translating all of this
   the important part is how [octantDistances] is called *)
let nearest ot pt =
  match ot with
  | (Leaf _) -> ()
  | (Node node) -> begin
      let distances = octantDistances @@ (V3.sub pt node.split) in
      ignore @@ distances;
      ()
    end

(* unused *)

(* gives octant of a first vector relative to the second vector as a center *)
let cmp ca cb =
  let cx = V3.x ca >= V3.x cb
  and cy = V3.y ca >= V3.y cb
  and cz = V3.z ca >= V3.z cb
  in
  joinStep (cx, cy, cz)

let octreeStep ot = function
  | NWU -> ot.nwu
  | NWD -> ot.nwd
  | NEU -> ot.neu
  | NED -> ot.ned
  | SWU -> ot.swu
  | SWD -> ot.swd
  | SEU -> ot.seu
  | SED -> ot.sed
