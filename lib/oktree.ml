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
    depth : int;
    size : float;
    origin : vec3;
    root : node;
  }
  and node = {
    mutable children: children;
    level: int;
    id : int;
    offset: vec3;
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
  val node_nearest : float -> vec3 -> node -> vec3 -> vec3
  val nearest : t -> vec3 -> vec3
  val distances : t -> vec3 -> (vec3 * float) list
  type tree_stats = {
    points : int;
    children : tree_stats option array;
    min_points_per_leaf : int option;
    max_points_per_leaf : int option;
    avg_points_per_leaf : float option;
  }
  val pp_tree_stats : Format.formatter -> tree_stats -> unit
  val stats : t -> tree_stats
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
  See https://docs.rs/charcoal/1.0.0/charcoal/ maybe?
  "trees use some sort of backing storage to store the elements, typically a
  Vec (or its variants, like SmallVec or ArrayVec), and instead of using
  pointers to link to children, indices into the storage are used instead"

  TODO:
  uses for / methods of octrees other than nearest neighbour search:
  - point membership:
    simplest one is "does point x exist in the point set?" i.e. no
    priority queue or octant distances needed, just go to the target leaf and
    check its point-set.
  - ray intersection:
    https://daeken.svbtle.com/a-stupidly-simple-fast-octree-traversal-for-ray-intersection
    http://bertolami.com/files/octrees.pdf
    Note: I think for both of these the tree stores triangles or polygons
    rather than points... or possibly it treats the octants themselves as
    the polygons, i.e. bounding boxes?
    a ray is "...best defined in parameterized form as a point (X0, Y0, Z0)
    and a direction vector (Dx, Dy, Dz)" seems like the line join those is
    like a 'unit length', you can keep adding D to the line to extend it.
    https://en.wikipedia.org/wiki/Ray_casting
  - collision detection:
    more complicated... find points or polygons in the tree which are inside
    or intersect with the given bounding box (or other polygon)
    https://www.gamedev.net/tutorials/programming/general-and-gameplay-programming/introduction-to-octrees-r3529/
    ...lists four kinds:
    - Frustum intersections (i.e. camera view field)
    - Ray intersections (as above)
    - Bounding Box intersections
    - Bounding Sphere Intersections
  - update value of a point:
    i.e. you have a scene where things are moving, either by mutation or
    replacement. It will have to move octants, and prev one may now be empty.
*)
module Make = functor (V3 : Vec3) ->
struct
  type vec3 = V3.t

  (* let pp_vec3 = V3.pp *)
  let pp_vec3 fmt p =
    let x, y, z = V3.to_tuple p in
    Format.fprintf fmt "(%f, %f, %f)" x y z


  type t = {
    depth: int;
    size: float;
    origin: vec3;
    root: node;
  }
  and node = {
    mutable children: children;
    level: int;  (* root is 0 *)
    id : int;
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

  let max_depth = 20  (* see: [id_to_string] *)

  let default_origin = V3.of_tuple (0., 0., 0.)
  let default_size = 1.

  let empty_node_children () = Nodes (Array.make 8 None)
  let empty_leaf_children () = Points []

  let root_children = function
    | d when d = 1 -> empty_leaf_children ()
    | _ -> empty_node_children ()

  (* init an octree root *)
  let empty ?(size=default_size) ?(origin=default_origin) depth =
    match depth with
    | d when d < 1 -> raise @@ Invalid_argument "depth must be >= 1"
    | d when d > max_depth ->
      raise @@ Invalid_argument (Printf.sprintf "depth must be <= %i" max_depth)
    | _ ->
      let root =
        {
          children = root_children depth;
          level = 0;
          id = 0;
          offset = V3.of_tuple (0., 0., 0.);
        }
      in
      {
        depth;
        origin;
        size;
        root;
      }

  let repeat_3 a = (a, a, a)

  let octant_size root_size level = root_size /. 2. ** (Float.of_int level)

  (*
    get index of child octant within [parent] that would contain [p]
    NOTE: result won't be meaningful if [p] is outside [parent]
  *)
  let octant_index_for_p root_size parent p =
    let subdiv = octant_size root_size (parent.level + 1) in
    let mask getter mask =
      let mid = getter parent.offset +. subdiv in
      match getter p with
      | a when a < mid -> 0
      | _ -> mask
    in
    (mask V3.x 4) lor (mask V3.y 2) lor (mask V3.z 1)

  (* get offset value for child octant at [index] in [parent] *)
  let octant_offset root_size parent index =
    let offset_size = octant_size root_size (parent.level + 1) in
    let offset getter mask =
      match index land mask with  (* 0 or 1 *)
      | 0 -> getter parent.offset
      | _ -> getter parent.offset +. offset_size
    in
    V3.of_tuple ((offset V3.x 4), (offset V3.y 2), (offset V3.z 1))

  let id_to_level id =
    (Float.log2 (Float.of_int @@ id + 1)) /. 3.
    |> Float.ceil
    |> Int.of_float

  (*
    id_to_path 234 |> Seq.map id_to_string |> List.of_seq;;
    - : string list = ["010"; "101010"; "011101010"]

    TODO: this doesn't look like I was intending, the shorter ones
    are truncated from the wrong end?

    I think this is what we want, each path uniquely locates
    the octant or level-parent of the octant matching [id]

    alternatively, single 'column' masks are:
    <this level mask> - <prev level mask>
    e.g.
    level 2 column mask is: 63 - 7 = 56    (0b000111000)
    level 3 column mask is: 511 - 63 = 448 (0b111000000)
  *)
  let id_to_path level id =
    let mask level = Int.of_float (8. ** (Float.of_int level)) - 1 in
    Seq.unfold (function
        | l when l <= level -> Some (id land mask l, l + 1)
        | _ -> None
      ) 1

  (*
    NOTE:
    this function and also use of bit-shifting elsewhere effectively
    puts a limit on max id value, and thus max level depth of the tree.
    See https://v2.ocaml.org/api/Int.html#VALshift_left)
    "The result is unspecified if n < 0 or n > Sys.int_size"

    On a 64 bit system:

      Sys.int_size = 63
      Int.max_int = 4611686018427387903
      Int.max_int |> Utils.log2 = 62
      2. ** 62. |> Int.of_float = -4611686018427387904  (overflow!!)
      2. ** 62. |> Int64.of_float = 4611686018427387904L
      2. ** 61. |> Int.of_float = 2305843009213693952   (no prob)

    In our case was also want a bit length that is multiple of 3.
    So depth=20 (60-bit ids) seems the limit using this approach.
    That gives 1_152_921_504_606_846_976 or ~1 quintillion octants.
  *)
  let id_to_string =
    (* each call to fun can re-use same buffer, this is basically a decorator *)
    let buf = Bytes.create Sys.int_size in
    let maxbits = Sys.int_size - 1 in
    fun level n ->
      if level = 0 then "" else
        begin
          for i = 0 to maxbits do
            let pos = maxbits - i in
            Bytes.set buf pos (if n land (1 lsl i) != 0 then '1' else '0')
          done;
          (* truncate from the left to a multiple of 3 bit length *)
          (* let len =
             let l = Utils.log2 (level + 1) |> Float.of_int in
             ((l /. 3.) |> Float.ceil) *. 3.
             |> Int.of_float
             in *)
          let len = level * 3 in
          Bytes.sub_string buf (maxbits - len + 1) len
        end

  let id_to_path_str level id =
    Seq.mapi (fun i -> id_to_string (i + 1)) (id_to_path level id)
    |> List.of_seq
    |> String.concat ","

  (* add a new leaf to the octree (updates root in place) *)
  let add tree p =
    let rec construct parent p =
      match parent.children with
      | Nodes nodes ->
        (* inner node *)
        let level = parent.level + 1 in
        let index = octant_index_for_p tree.size parent p in
        let offset = octant_offset tree.size parent index in
        let id = (parent.id lsl 3) lor index in
        let children =
          match level with
          | l when l < tree.depth -> empty_node_children ()
          | _ -> empty_leaf_children ()
        in
        let node = begin
          match Array.get nodes index with
          | Some n -> n  (* target node already exists *)
          | None ->
            {
              children;
              level;
              id;
              offset;  (* relative to root *)
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

  let of_list ?(size=default_size) ?(origin=default_origin) depth l =
    let tree = empty ~size ~origin depth in
    List.iter (add tree) l;
    tree

  let of_seq ?(size=default_size) ?(origin=default_origin) depth s =
    let tree = empty ~size ~origin depth in
    Seq.iter (add tree) s;
    tree

  (*
    recursively collect the points below a given octant
    (I'm not sure this has much utility beyond debugging)
  *)
  let points node =
    let rec collect points children =
      match children with
      | Nodes nodes ->
        Array.fold_left (fun points' child ->
            match child with
            | Some node -> collect points' node.children
            | None -> points'
          ) points nodes
      | Points points' -> List.append points points'
    in
    collect [] node.children

  (* Euclidean distance. Always positive (i.e. has no direction) *)
  let distance a b = V3.sub a b |> V3.norm
  (* Possible optimisation: https://stackoverflow.com/a/1678481/202168
     if we only need to 'sort' and don't care about magnitude of
     distances then the sqrt step is superfluous (I think that
     would be just V3.norm2) *)

  let box origin size =
    let x0, y0, z0 = V3.to_tuple origin in
    let x1, y1, z1 = x0 +. size, y0 +. size, z0 +. size in
    Array.init 8 (function
        | 0 -> V3.of_tuple (x0, y0, z0)
        | 1 -> V3.of_tuple (x0, y0, z1)
        | 2 -> V3.of_tuple (x0, y1, z0)
        | 3 -> V3.of_tuple (x0, y1, z1)
        | 4 -> V3.of_tuple (x1, y0, z0)
        | 5 -> V3.of_tuple (x1, y0, z1)
        | 6 -> V3.of_tuple (x1, y1, z0)
        | 7 -> V3.of_tuple (x1, y1, z1)
        | _ -> raise Not_found
      )

  type v3_array = vec3 array [@@deriving show]
  type v3_list = vec3 list [@@deriving show]

  (*
    Find the distance from [p] to the nearest face, edge or vertex of [octant]
    Method from: https://math.stackexchange.com/a/2133235/181250
  *)
  let octant_surface_distance root_size root_origin octant p =
    (*
      We want to translate our octant so that its centre lies at 0,0
      (makes the big if/else below simpler)

      We don't actually translate the octant, but rather the point [p]
      and then calculate surface distance as if octant was translated.

      TODO: scaled octant offsets could be pre-calculated, the scalings
      are different for each octant but they are static

      TODO: point is in wrong octant
      y and z values are bigger than the bounds

      | Octant(level:3, id:001000001):
      | offset by: (-0.031250, -0.031250, -0.656250)
      | (0.000000, 0.000000, 0.625000)  -->  (-0.031250, -0.031250, -0.031250)
      |<(0.062500, 0.062500, 0.687500)> --> <(0.031250, 0.031250, 0.031250)>
      | Octant points'
      [(0.040810, 0.120397, 0.712801)] --> [(0.009560, 0.089147, 0.056551)]
      | Point: (0.333300, 0.410000, 0.666700) --> (0.302050, 0.378750, 0.010450)
      | Brute-force d: 0.414180
    *)
    let octsize = octant_size root_size octant.level in
    let s = octsize /. 2. in
    (* Format.printf "| s: %f\n" s; *)
    let oct_centre_offset = V3.add octant.offset (V3.of_tuple (repeat_3 s)) in
    (* root origin is a distance from 0,0, octant.offset is relative to root origin *)
    let root_offset = V3.sub (V3.of_tuple (0., 0., 0.)) root_origin in
    let offset = V3.sub root_offset oct_centre_offset in
    let p' = V3.add p offset in
    (* Format.printf "| Octant(level:%i, id:%s):\n| offset by: %a\n"
       octant.level
       (id_to_string octant.level octant.id)
       pp_vec3 offset;
       Format.printf "| %a  -->  %a\n" pp_vec3 octant.offset pp_vec3 (V3.add octant.offset offset); *)
    let far_corner =
      (V3.add
         octant.offset
         (V3.of_tuple (repeat_3 octsize)))
    in
    (* Format.printf "|<%a> --> <%a>\n" pp_vec3 far_corner pp_vec3 (V3.add far_corner offset); *)
    let offset_points =
      List.map (fun p'' ->
          V3.add p'' offset
        ) (points octant)    
    in
    (* Format.printf "| Octant points' %a --> %a\n" pp_v3_list (points octant) pp_v3_list offset_points;
       Format.printf "| Point: %a --> %a\n" pp_vec3 p pp_vec3 p'; *)
    let brute_force =
      List.map (distance p') offset_points
      |> List.sort compare
      |> List.hd
    in
    (* Format.printf "| Brute-force d: %f\n" brute_force; *)
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
    let d =
      if x <= s then
        if y <= s then
          (* without the max this would give negative distance in case where
             p is in octant - that would actually still work with our pq
             TODO test edge case where point is on boundary ... can we
             ever get a point and an adjacent octant both returning 0. ? *)
          begin
            (* face *)
            (* Format.printf "| case 1: x <= s y <= s : z - s\n"; *)
            max 0. (z -. s)  (* 0 if p in octant, all other cases return > 0 *)
          end
        else
        if z <= s then
          begin
            (* face *)
            (* Format.printf "| case 2: x <= s z <= s : y - s\n"; *)
            y -. s
          end
        else
          begin
            (* edge *)
            (* Format.printf "| case 3: x <= s : √(y-s)^2+(z-s)^2\n"; *)
            sqrt ((y -. s) ** 2. +. (z -. s) ** 2.)
          end
      else
      if y <= s then
        if z <= s then
          begin
            (* face *)
            (* Format.printf "| case 4: y <= s z <= s : x - s\n"; *)
            x -. s
          end
        else
          begin
            (* edge *)
            (* Format.printf "| case 5: y <= s : √(x-s)^2+(z-s)^2\n"; *)
            sqrt ((x -. s) ** 2. +. (z -. s) ** 2.)
          end
      else
      if z <= s then
        begin
          (* edge *)
          (* Format.printf "| case 6: z <= s : √(x-s)^2+(y-s)^2\n"; *)
          sqrt ((x -. s) ** 2. +. (y -. s) ** 2.)
        end
      else
        begin
          (* vertex *)
          (* Format.printf "| case 7: else : √(x-s)^2+(y-s)^2+(z-s)^2\n"; *)
          sqrt ((x -. s) ** 2. +. (y -. s) ** 2. +. (z -. s) ** 2.)
        end
    in
    (* Printf.printf "| Surface d: %f\n" d; *)
    d

  type child =
    | Node of node
    | Point of vec3
    (* [@@deriving show] *)

  type point_list = vec3 list [@@deriving show]

  (* make Priority Queue types for our nearest search *)
  module PQ_Item = struct
    type t = child
    let compare a b =
      (* PQ appears to treat items which compare equal as dups *)
      match a, b with
      | Node a', Node b' -> compare (a'.level, a'.id) (b'.level, b'.id)
      | Node _, Point _ -> 1
      | Point _, Node _ -> -1
      | Point a', Point b' -> V3.compare a' b'
    let pp fmt item =
      match item with
      | Node node ->
        let points = points node in
        Format.fprintf fmt
          "Node(level: %i, id: %s, points: %a, offset: %a)"
          node.level
          (id_to_string node.level node.id)
          pp_point_list points
          pp_vec3 node.offset
      (* pp_node fmt node *)
      | Point point -> Format.fprintf fmt "Point%a" pp_vec3 point
  end
  module PQ_Priority = struct
    type t = float
    let compare = compare
    let pp fmt = Format.fprintf fmt (format_of_string "%f")
  end
  module PQ = Psq.Make(PQ_Item)(PQ_Priority)

  let pp_pq_pair fmt pair =
    let item, d = pair in
    Format.fprintf fmt "(%a, distance: %a)" PQ_Item.pp item PQ_Priority.pp d

  let children_to_seq = function
    | Nodes nodes ->
      Array.to_seq nodes
      |> Seq.filter_map (function
          | Some node -> Some (Node node)
          | None -> None
        )
    | Points points -> List.to_seq points |> Seq.map (fun el -> Point el)

  let _nearest points p =
    if List.length points = 0 then
      raise @@ Invalid_argument "nearest oracle: points list was empty"
    else
      let d, _ =
        List.map (fun p' -> (distance p' p, p')) points
        |> List.sort compare
        |> List.hd
      in
      d

  (* TODO return option type instead of raise Not_found ? *)
  let node_nearest root_size root_offset node p =
    let rec nearest' pq children p =
      (* enqueue children of current octant *)
      let items = Seq.map (fun child ->
          match child with
          | Node node ->
            let d = octant_surface_distance root_size root_offset node p in
            let d' =
              List.map (distance p) (points node)
              |> List.sort compare
              |> List.hd
            in
            let _ =
              if d' < d then
                (* raise @@ Invalid_argument
                   (Printf.sprintf "Bruce force %f < Oct-surface %f" d' d) *)
                Printf.printf ":: Bruce force %f < Oct-surface %f\n" d' d
            in
            (Node node, d)
          | Point p' -> 
            let d = distance p p' in
            (Point p', d)
        ) children
      in
      let _items = List.of_seq items in
      let pq = PQ.add_seq items pq in
      (* Format.printf "PQ: %a\n" (PQ.pp_dump PQ_Item.pp PQ_Priority.pp) pq; *)
      (* pop current best match *)
      match PQ.pop pq with
      | Some ((child, _), pq) -> begin
          match child with
          | Point p' -> p'  (* this is the nearest *)
          | Node node -> nearest' pq (children_to_seq node.children) p
        end
      | None -> raise Not_found  (* would mean our tree was empty *)
    in
    nearest' PQ.empty (children_to_seq node.children) p

  type tree_stats = {
    points : int;
    children : tree_stats option array;
    min_points_per_leaf : int option;
    max_points_per_leaf : int option;
    avg_points_per_leaf : float option;
  }
  [@@deriving show]

  let stats tree =
    let stats_fold f i children =
      Array.fold_left (fun a child ->
          match child with
          | Some ts -> f a ts
          | None -> a
        ) i children
    in
    let rec stats' parent =
      let points = List.length @@ points parent in
      let children =
        match parent.children with
        | Nodes nodes -> begin
            Array.map (fun a ->
                match a with
                | Some node -> Some (stats' node)
                | None -> None
              ) nodes
          end
        | Points _ -> Array.make 8 None
      in
      match parent.level with
      | l when l < tree.depth ->
        let min_points_per_leaf =
          stats_fold (fun a ts ->
              let cmp_val =
                match ts.min_points_per_leaf with
                | Some _ as mppl -> mppl
                | None -> Some 0
              in
              match compare cmp_val a with
              | -1 -> cmp_val
              | _ -> a
            ) (Some 0) children
        in
        let max_points_per_leaf =
          stats_fold (fun a ts ->
              let cmp_val =
                match ts.max_points_per_leaf with
                | Some _ as mppl -> mppl
                | None -> Some ts.points
              in
              match compare cmp_val a with
              | 1 -> cmp_val
              | _ -> a
            ) (Some 0) children
        in
        let avg_points_per_leaf =
          let sum = stats_fold (fun a ts -> a + ts.points) 0 children in
          Some ((Float.of_int sum) /. (Float.of_int points))
        in
        {
          points;
          children;
          min_points_per_leaf;
          max_points_per_leaf;
          avg_points_per_leaf;
        }
      | _ ->
        {
          points;
          children;
          min_points_per_leaf = None;
          max_points_per_leaf = None;
          avg_points_per_leaf = None;
        }
    in
    stats' tree.root

  let nearest tree p =
    node_nearest tree.size tree.origin tree.root p

  let distances tree p =
    List.map (fun p' -> (p', distance p p')) (points tree.root)

end
