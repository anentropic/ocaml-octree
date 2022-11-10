# NOTES

### TODO:
support non-cubes? as long as you are happy with euclidean
distance would it matter if space was cuboid rather than cube?
(~cuboid = "right rectangular prism")

Could we supply an alternative distance function?
https://machinelearningmastery.com/distance-measures-for-machine-learning

### TODO:
what if there are multiple equidistant matches? currently we just
return the first leaf that wins the priority queue - psq will compare
item values where their priority is equal, so we could tweak this to be
deterministic (e.g. favour darkest or brightest value) or keep popping
all the equal priorities and return a set?

### TODO:
points should be unique? - currently no validation for duplicates
(we may return one or all depending on equidistant behaviour)

### TODO:
question - would it be in any way more efficient/better to store the
tree as a 2D array on root instead of nested array-in-a-record-field ?
See https://docs.rs/charcoal/1.0.0/charcoal/ maybe?
"trees use some sort of backing storage to store the elements, typically a
Vec (or its variants, like SmallVec or ArrayVec), and instead of using
pointers to link to children, indices into the storage are used instead"

### TODO:
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
- https://en.wikipedia.org/wiki/Computational_geometry#Geometric_query_problems  

See e.g. https://hackage.haskell.org/package/Octree-0.6.0.1/docs/Data-Octree.html
https://github.com/BioHaskell/octree/blob/master/Data/Octree/Internal.hs
- is generic on some opaque 'payload' that is stored alongside the point  
...is that useful? seems likely to spoil memory usage
- works without specifying any root size
- octant distance calcs looks similar... but different?
    - reformulated as a series of >= 0 checks instead of <= size checks
    - nice
    - had to rewrite this in OCaml to understand what's going on
- implementation does not seem to use pre-chosen depth, instead points are added
to a leaf until it hits a limit (16) and then leaf is split adding a level
...is this performance-tuned? i.e. it's as cheap to brute-force 16 points
as to do 8 octants and then down a level?
- Nodes only need a 'split point' V3 attribute... this condenses size and offset
into one - I guess we don't need outer boundaries since we already split down
from parent node. Our 'origin' is the parent's split point. Is theirs the same?
- in case of creation from list: root origin is chosen by 'mass centre' of the
whole list of points (rather than specified as an arg)

## Methods:
(from the Haskell implementation above)
- `delete` and `deleteBy` (the latter using a test function)
- `depth` and `size` (I guess size is a count of points?)
- `withinRange`: "all points within a given distance from argument"
- **Functor**
    - `map`
    - `<$` (seems to be like `init` or `fill` with a single value)
        ...we could also mimic some of the OCaml init styles e.g. via function
- **Foldable**
    - various vaiants of `fold` (left, right, fold+map etc)
    - some pre-defined folds (`length`, `max`, `min`, `sum`, `product`)
    - something like `exists` but using a test function
    - `to_list` shows up here (which is just our `points`' method)
    - `null` ...could have been named 'is_empty'
- **Traversable**  
    this is the more interesting one
    > "Class of data structures that can be traversed from left to right, performing
    > an action on each element."
    this sounds a lot like map or fold, but the "actions" are either Applicative
    or Monad ... not quite sure what that means in practice though
    - see https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Traversable.html#v:traverse
        for description of the methods
    - `traverse_` (discarding results) seems a bit like `iter` in OCaml, but monadic
    Haskell docs give a Tree type as an example for Traversable:
    ```haskell
        instance Traversable Tree where
            traverse f Empty = pure Empty
            traverse f (Leaf x) = Leaf <$> f x
            traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
    ```

### See also:
- https://github.com/ocaml-ppx/ppx_deriving#plugins-iter-map-and-fold
- https://hackage.haskell.org/package/filtrable-0.1.1.0/docs/Data-Filtrable.html
- https://github.com/thierry-martinez/traverse
- https://mattwindsor91.github.io/travesty/travesty/Travesty/Traversable/index.html
- https://blog.shaynefletcher.org/2017/05/more-type-classes-in-ocaml.html  
    various including *Applicative* and *Traversable*
    
    > "It may be described as a generalization of the iterator pattern"
    
    > "traverse in this context is a function that allows us to map each element of a
    > list to an optional value where the computation produces a list with all values
    > collected, only in case every element was successfully mapped to a Some value."
    
    i.e. filter_map
- https://discuss.ocaml.org/t/notes-from-compose-2017/240/6?u=anentropic  
    shows trivial-looking implementation of *Applicative* and *Traversable*
- It's probably a https://wiki.haskell.org/Monoid too?
- https://apfelmus.nfshost.com/articles/monoid-fingertree.html  
    > "binary search on monoids [is a] powerful tool to construct about any fancy
    > data structure with logarithmic access time that you can imagine"
- https://academickids.com/encyclopedia/index.php/Semilattice  
    Seems to basically be about ordered sets.  
    It's a narrowing of *Semigroup* (which provides `empty` `append` and `concat`)  
    Squinting at various Haskell libs it seems to imply operations like:
    - set union, or `merge`
    - `min`, `max`, `any`, `all`, `lt`, `gt`
    
    > "Any tree structure (with the root as the least element) is a meet-semilattice."

    Wikipedia words it a bit more precisely:

    > "Any single-rooted tree (with the single root as the least element) of height ≦ ⍵
    > is a (generally unbounded) meet-semilattice. Consider for example the set of
    > finite words over some alphabet, ordered by the prefix order. It has a least
    > element (the empty word), which is an annihilator element of the meet operation,
    > but no greatest (identity) element."
    
    at the same time...  
    https://www.reddit.com/r/compsci/comments/55rzyk/comment/d8d9os9/

    > "logical AND operator is a semilattice because it obeys three useful laws"
    
    (associativity, commutativity, idempotency)
    
    it's weird that a data structure and a logical operator can both be this thing.
    
    Is our Octree a meet-semilattice? i.e. is there some associative, commutative,
    idempotent operator we can use to compare nodes where the root node will always
    compare as 'least'?  Maybe only the Nodes and not the Points?
    
    > "...lattice theory's root as a tool to explain how to derive order relations
    >(akin to <= and >=) on arbitrary structures"