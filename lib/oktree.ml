(*
  A type for the points in our octree

  Will essentially be a vector of three floats
  e.g. https://erratique.ch/software/gg/doc/Gg/V3/
*)
module type Vec3 = sig
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

(*
  Sparse octree where leaves are Vec3 points in 3D space
  (octants without leaves are omitted)

  the octree will be an 'axis-aligned' cube
  and all three axes will have the same scale
  origin and size are configurable, as are number of 'levels'
  i.e. subdivisions

  TODO: support non-cubes? as long as you are happy with euclidean
  distance would it matter if space was cuboid rather than cube?
  (~cuboid = "right rectangular prism")

  Could we supply an alternative distance function?
  https://machinelearningmastery.com/distance-measures-for-machine-learning
*)
module Make = functor (V3 : Vec3) ->
struct
  type vec3 = V3.t

  type t =
    | Leaf of vec3
    | Node of node
  and node = {
    children: t option array;
    level: int;
    offset: vec3;
    (*
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
  }

  type root = {
    max_depth: int;
    size: float;
    origin: vec3;
    tree: node;
  }

  let default_origin = V3.of_tuple (0., 0., 0.)
  let default_size = 1.

  (* init an octree root *)
  let empty ?(size=default_size) ?(origin=default_origin) max_depth =
    match max_depth with
    | d when d < 1 -> raise @@ Invalid_argument "max_depth must be >= 1"
    | _ ->
      {
        max_depth;
        origin;
        size;
        tree = {
          children = Array.make 8 None;
          level = 1;
          offset = V3.of_tuple (0., 0., 0.);
        }
      }

  let normalise_origin = V3.of_tuple (-1., -1., -1.)
  let normalise_size = 2.

  let repeat_3 a = (a, a, a)

  let normalise_scale root_size root_origin =
    let base_size = V3.of_tuple (repeat_3 normalise_size) in
    let coord_size = V3.of_tuple (repeat_3 root_size) in
    V3.div base_size (V3.sub coord_size root_origin)

  let normalise_offset root_origin =
    V3.sub normalise_origin root_origin

  (*
    get index of child octant within [parent] that would contain [p]
    NOTE: result won't be meaningful if [p] is outside [parent]
  *)
  let octant_index_for_p parent p =
    let subdiv = 1. /. 2. ** (Float.of_int parent.level) in
    let mask getter mask =
      let mid = getter parent.offset +. subdiv in
      match getter p with
      | a when a < mid -> 0
      | _ -> mask
    in
    (mask V3.x 4) lor (mask V3.y 2) lor (mask V3.z 1)

  (* get offset value for child octant at [index] in [parent] *)
  let octant_offset parent index =
    let offset_size = 1. /. 2. ** (Float.of_int parent.level) in
    let offset getter mask =
      match index land mask with  (* 0 or 1 *)
      | 0 -> getter parent.offset
      | _ -> getter parent.offset +. offset_size
    in
    V3.of_tuple ((offset V3.x 4), (offset V3.y 2), (offset V3.z 1))

  (* add a new leaf to the octree (updates root in place) *)
  let add root p =
    let rec construct tree p =
      let index = octant_index_for_p tree p in
      let get_or_make_node level =
        (* if we're calling this there should be no leaves at this level *)
        match Array.get tree.children index with
        | Some (Node n) -> n
        | Some (Leaf _) -> raise @@ Invalid_argument "Leaf not expected"
        | None ->
          let offset = octant_offset tree index in
          {
            children = Array.make 8 None;
            level;
            offset;
          }
      in
      match tree.level with
      | l when l < root.max_depth ->
        let node = get_or_make_node (tree.level + 1) in
        let constructed = construct node p in
        Array.set tree.children index (Some (Node constructed));
        tree
      | _ ->
        Array.set tree.children index (Some (Leaf p));
        tree
    in
    ignore @@ construct root.tree p

  let of_list ?(size=default_size) ?(origin=default_origin) max_depth l =
    let root = empty ~size ~origin max_depth in
    List.iter (add root) l;
    root

  let of_seq ?(size=default_size) ?(origin=default_origin) max_depth s =
    let root = empty ~size ~origin max_depth in
    Seq.iter (add root) s;
    root

  (*
    recursively collect the leaves below a given octant
    (I'm not sure this has much utility beyond debugging)
  *)
  let leaves tree =
    let rec collect leaves children =
      Array.fold_left (fun leaves child ->
          match child with
          | Some (Leaf l) -> List.cons l leaves
          | Some (Node n) -> collect leaves n.children
          | None -> leaves
        ) leaves children
    in
    collect [] tree.children

  (* Euclidean distance. Always positive (i.e. has no direction) *)
  let distance a b = V3.sub a b |> V3.norm

  (*
    Find the distance from [p] to the nearest face, edge or vertex of [octant]

    Method from: https://math.stackexchange.com/a/2133235/181250
  *)
  let octant_min_distance root_size root_origin octant p =
    (*
      Scale and translate p with octant so that octant coords cover -1,-1,-1...1,1,1
      which is needed for the octant_min_distance function
      (e.g. by default our root octant is natively 0,0,0...1,1,1)

      e.g. level 2, octant dimensions are 0.5*0.5*0.5
      scale by 2*2=4 times
      octant 000 grows to cover 0,0,0...2,2,2
      offset by -1,-1,-1
    *)
    let level' = Float.of_int octant.level in
    let p_to_normalised octant p =
      let level_vec3 = V3.of_tuple (repeat_3 level') in
      let scale' v3 = V3.mul v3 level_vec3 |> V3.mul (normalise_scale root_size root_origin) in
      let offset = V3.sub (normalise_offset root_origin) (scale' octant.offset) in
      V3.add (scale' p) offset
    in
    let x, y, z = p_to_normalised octant p |> V3.map abs_float |> V3.to_tuple in
    (*
      For this calculation the point [p] has one of 27 possible positions
      in relation to the octant, which correspond to the 26 adjacent octants
      plus potentially being within the target octant.
      26 adjacent = 6 face:face + 8 vertex:vertex + 12 edge:edge
      By taking |abs| values these are collapsed down to the 7+1 cases below,
      finding the distance to nearest face, vertex or edge as appropriate.
      (I think these are Euclidean distance too)
    *)
    let d =
      if x <= 1. then
        if y <= 1. then
          (* without the max this would give negative distance in case where
             p is in octant - that would actually still work with our pq
             TODO test edge case where point is on boundary ... can we
             ever get a point and an adjacent octant both returning 0. ? *)
          max 0. (z -. 1.)  (* 0 if p in octant, all other cases return > 0 *)
        else
        if z <= 1. then
          y -. 1.
        else
          sqrt (y -. 1.) ** 2. +. (z -. 1.) ** 2.
      else
      if y <= 1. then
        if z <= 1. then
          x -. 1.
        else
          sqrt ((x -. 1.) ** 2. +. (z -. 1.) ** 2.)
      else
      if z <= 1. then
        sqrt ((x -. 1.) ** 2. +. (y -. 1.) ** 2.)
      else
        sqrt ((x -. 1.) ** 2. +. (y -. 1.) ** 2. +. (z -. 1.) ** 2.)
    in
    (* denormalise, so we can compare octant distances with p2p distances *)
    d /. level' /. normalise_size

  (* make a Priority Queue type for our nearest search *)
  type _octree_t = t
  module PQ_Item = struct
    type t = _octree_t
    let compare = compare
  end
  module PQ_Priority = struct
    type t = float
    let compare = compare
  end
  module PQ = Psq.Make(PQ_Item)(PQ_Priority)

  let tree_nearest root_size root_offset tree p =
    let rec nearest' pq tree p =
      (* enqueue children of current octant *)
      let pq = PQ.add_seq (
          Array.to_seq tree.children
          |> Seq.filter_map Fun.id  (* omit Nones *)
          |> Seq.map (fun child ->
              match child with
              | Leaf l ->
                let d = distance l p in
                (child, d)
              | Node n ->
                let d = octant_min_distance root_size root_offset n p in
                (child, d)
            )
        ) pq
      in
      (*
        pop current best match
        if we pushed any leaves above we are likely in the octant of the best
        match and best leaf could be our match, or else if an adjacent node
        (will have been already pushed) has smaller min distance than any leaf
        in the queue we will pop the node instead and explore that... meaning
        if we ever pop a leaf here it should be our best match
      *)
      match PQ.pop pq with
      | Some ((tree, _), pq) -> begin
          match tree with
          | Leaf l -> l
          | Node n -> nearest' pq n p
        end
      | None -> raise Not_found  (* would mean our tree is empty *)
    in
    nearest' PQ.empty tree p

  let nearest root p =
    tree_nearest root.size root.origin root.tree p

end
