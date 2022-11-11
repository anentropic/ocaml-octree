(*
  A type for the points in our octree

  Will essentially be a vector of three floats
  e.g. https://erratique.ch/software/gg/doc/Gg/V3/
*)
module type VEC3 = sig
  type t

  val x : t -> float
  (** [x v] is the x component of [v]. *)

  val y : t -> float
  (** [y v] is the y component of [v]. *)

  val z : t -> float
  (** [z v] is the z component of [v]. *)

  val of_tuple : (float * float * float) -> t
  (** [of_tuple (x, y, z)] is [v x y z]. *)

  val to_tuple : t -> (float * float * float)
  (** [to_tuple v] is [(x v, y v, z v)]. *)

  val add : t -> t -> t
  (** [add u v] is the vector addition [u + v]. *)

  val sub : t -> t -> t
  (** [sub u v] is the vector subtraction [u - v]. *)

  val mul : t -> t -> t
  (** [mul u v] is the component wise multiplication [u * v]. *)

  val div : t -> t -> t
  (** [div u v] is the component wise division [u / v]. *)

  val norm : t -> float
  (** [norm v] is the norm [|v| = sqrt v.v]. *)

  val map : (float -> float) -> t -> t
  (** [map f v] is the component wise application of [f] to [v]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf v] prints a textual representation of [v] on [ppf]. *)

  val compare : t -> t -> int
  (** [compare u v] is [Stdlib.compare u v]. *)
end

(* public interface for Oktrees *)
module type OKTREE =
sig
  type vec3
  type t = {
    depth : int;   (* number of subdivisions *)
    size : float;  (* max value - min value *)
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