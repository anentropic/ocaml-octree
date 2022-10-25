module type Vec3 =
sig
  type t
  val x : t -> float
  val y : t -> float
  val z : t -> float
  val of_tuple : float * float * float -> t
  val to_tuple : t -> float * float * float
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val norm : t -> float
  val map : (float -> float) -> t -> t
end

module type Octree =
sig
  type vec3
  type t = Leaf of vec3 | Node of node
  and node = { children : t option array; level : int; offset : vec3; }
  type root = {
    max_depth : int;
    size : float;
    origin : vec3;
    tree : node;
  }
  val empty : ?size:float -> ?origin:vec3 -> int -> root
  val add : root -> vec3 -> unit
  val of_list : ?size:float -> ?origin:vec3 -> int -> vec3 list -> root
  val of_seq : ?size:float -> ?origin:vec3 -> int -> vec3 Seq.t -> root
  val leaves : node -> vec3 list
  val tree_nearest : float -> vec3 -> node -> vec3 -> vec3
  val nearest : root -> vec3 -> vec3
end

module Make :
  functor (V3 : Vec3) -> Octree with type vec3 = V3.t
