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
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
end

module type M =
sig
  type vec3
  type t = {
    depth : int;
    size : float;
    root : node;
  }
  and node = {
    mutable children: children;
    level: int;
    id : int;
    origin: vec3;
  }
  and children =
    | Nodes of node option array
    | Points of vec3 list
  val pp : Format.formatter -> t -> unit
  val pp_node : Format.formatter -> node -> unit
  val pp_children : Format.formatter -> children -> unit
  val empty : ?size:float -> ?origin:vec3 -> int -> t
  val add : t -> vec3 -> unit
  val of_list : ?size:float -> ?origin:vec3 -> int -> vec3 list -> t
  val of_seq : ?size:float -> ?origin:vec3 -> int -> vec3 Seq.t -> t
  val points : node -> vec3 list
  val node_nearest : float -> node -> vec3 -> vec3
  val nearest : t -> vec3 -> vec3
  val distances : t -> vec3 -> (vec3 * float) list
end

module Make :
  functor (V3 : Vec3) -> M with type vec3 = V3.t
