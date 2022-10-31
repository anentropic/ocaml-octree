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

  val pp : Format.formatter -> t -> unit
  (** [pp ppf v] prints a textual representation of [v] on [ppf]. *)

  val compare : t -> t -> int
  (** [compare u v] is [Stdlib.compare u v]. *)
end

module type M =
sig
  type vec3
  type t = {
    max_depth : int;
    size : float;
    origin : vec3;
    root : node;
  } and
  node = {
    mutable children: children;
    level: int;
    offset: vec3;
  } and
  children =
      | Nodes of node option array
      | Points of vec3 list
  val pp : Format.formatter -> t -> unit
  val pp_node : Format.formatter -> node -> unit
  val pp_children : Format.formatter -> children -> unit
  val empty : ?size:float -> ?origin:vec3 -> int -> t
  val add : t -> vec3 -> unit
  val of_list : ?size:float -> ?origin:vec3 -> int -> vec3 list -> t
  val of_seq : ?size:float -> ?origin:vec3 -> int -> vec3 Seq.t -> t
  val leaves : node -> vec3 list
  val node_nearest : float -> vec3 -> node -> vec3 -> vec3
  val nearest : t -> vec3 -> vec3
  val distances : t -> vec3 -> (vec3 * float) list
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

  TODO: what if there are multiple equidistant matches? currently we just
  return the first leaf that wins the priority queue - psq will compare
  item values where their priority is equal, so we could tweak this to be
  deterministic (e.g. favour darkest or brightest value) or keep popping
  all the equal priorities and return a set?

  TODO: points should be unique? - currently no validation for duplicates
  (we may return one or all depending on equidistant behaviour)

  TODO: question - would it be in any way more efficient/better to store the
  tree as a 2D array on root instead of nested array-in-a-record-field ?
*)
module Make = functor (V3 : Vec3) ->
struct
  type vec3 = V3.t

  let pp_vec3 = V3.pp

  type t = {
    max_depth: int;
    size: float;
    origin: vec3;
    root: node;
  }
  and node = {
    mutable children: children;
    level: int;  (* node levels start at 1 *)
    offset: vec3;
  }
  and children =
    | Nodes of node option array
    | Points of vec3 list 
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

  let default_origin = V3.of_tuple (0., 0., 0.)
  let default_size = 1.

  let empty_node_children () = Nodes (Array.make 8 None)
  let empty_leaf_children () = Points []

  let root_children = function
    | d when d = 1 -> empty_leaf_children ()
    | _ -> empty_node_children ()

  (* init an octree root *)
  let empty ?(size=default_size) ?(origin=default_origin) max_depth =
    match max_depth with
    | d when d < 1 -> raise @@ Invalid_argument "max_depth must be >= 1"
    | _ ->
      let root =
        {
          children = root_children max_depth;
          level = 1;
          offset = V3.of_tuple (0., 0., 0.);
        }
      in
      {
        max_depth;
        origin;
        size;
        root;
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
  let add tree p =
    let rec construct parent p =
      match parent.children with
      | Nodes nodes ->
        (* inner node *)
        let level = parent.level + 1 in
        let index = octant_index_for_p parent p in
        let offset = octant_offset parent index in
        let children =
          match level with
          | l when l < tree.max_depth -> empty_node_children ()
          | _ -> empty_leaf_children ()
        in
        let node = begin
          match Array.get nodes index with
          | Some n -> n  (* target node already exists *)
          | None ->
            {
              children;
              level;
              offset;
            }
        end
        in
        let node = construct node p in
        Array.set nodes index (Some node);
        parent
      | Points points ->
        (* leaf *)
        parent.children <- Points (List.cons p points);
        parent
    in
    ignore @@ construct tree.root p

  let of_list ?(size=default_size) ?(origin=default_origin) max_depth l =
    let tree = empty ~size ~origin max_depth in
    List.iter (add tree) l;
    tree

  let of_seq ?(size=default_size) ?(origin=default_origin) max_depth s =
    let tree = empty ~size ~origin max_depth in
    Seq.iter (add tree) s;
    tree

  (*
    recursively collect the leaves below a given octant
    (I'm not sure this has much utility beyond debugging)
  *)
  let leaves node =
    let rec collect leaves children =
      match children with
      | Nodes nodes ->
        Array.fold_left (fun leaves child ->
            match child with
            | Some node -> collect leaves node.children
            | None -> leaves
          ) leaves nodes
      | Points points -> List.append leaves points
    in
    collect [] node.children

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

      TODO: scaled octant offsets could be pre-calculated, the scalings
      are different for each octant but they are static
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

  type child =
    | Node of node
    | Point of vec3

  (* make Priority Queue types for our nearest search *)
  module PQ_Item = struct
    type t = child
    let compare a b =
      (* this will be used to distinguish equidistant items *)
      match a, b with
      | Node _, Node _ -> 0
      | Node _, Point _ -> 1
      | Point _, Node _ -> -1
      | Point _, Point _ -> 0
    let pp fmt item =
      match item with
      | Node node ->
        Format.fprintf fmt "Node(level: %i, offset: %a)" node.level V3.pp node.offset
      (* pp_node fmt node *)
      | Point point -> V3.pp fmt point
  end
  module PQ_Priority = struct
    type t = float
    let compare = compare
    let pp fmt = Format.fprintf fmt (format_of_string "%f")
  end
  module PQ = Psq.Make(PQ_Item)(PQ_Priority)

  let children_to_seq = function
    | Nodes nodes ->
      Array.to_seq nodes
      |> Seq.filter_map (function
          | Some node -> Some (Node node)
          | None -> None
        )
    | Points points -> List.to_seq points |> Seq.map (fun el -> Point el)

  (* TODO return option type instead of raise Not_found ? *)
  let node_nearest root_size root_offset node p =
    let rec nearest' pq children p =
      (* enqueue children of current octant *)
      let pq = PQ.add_seq (
          Seq.map (fun child ->
              match child with
              | Node node ->
                let d = octant_min_distance root_size root_offset node p in
                (Node node, d)
              | Point p' -> 
                let d = distance p p' in
                (Point p', d)
            ) children
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
      ignore @@ Format.printf "PQ: %a\n" (PQ.pp_dump PQ_Item.pp PQ_Priority.pp) pq;
      match PQ.pop pq with
      | Some ((child, _), pq) -> begin
          match child with
          | Point p' -> p'
          | Node node -> nearest' pq (children_to_seq node.children) p
        end
      | None -> raise Not_found  (* would mean our tree was empty *)
    in
    nearest' PQ.empty (children_to_seq node.children) p

  let nearest tree p =
    node_nearest tree.size tree.origin tree.root p

  let distances tree p =
    List.map (fun p' -> (p', distance p p')) (leaves tree.root)

end
