# NOTES

### TODO:
support non-cubes?  
as long as you are happy with euclidean distance would it matter if space was cuboid rather than cube?  
(~cuboid = "right rectangular prism")

Could we supply an alternative distance function?  
https://machinelearningmastery.com/distance-measures-for-machine-learning

### TODO:
what if there are multiple equidistant matches? currently we just return the first leaf that wins the priority queue - psq will compare item values where their priority is equal, so we could tweak this to be deterministic (e.g. favour darkest or brightest value) or keep popping all the equal priorities and return a set?

### TODO:
points should be unique? - currently no validation for duplicates  
(we may return one or all depending on equidistant behaviour)

### TODO:
question - would it be in any way more efficient/better to store the tree as a 2D array on root instead of nested array-in-a-record-field?

See https://docs.rs/charcoal/1.0.0/charcoal/ maybe? (for inspiration)

> "trees use some sort of backing storage to store the elements, typically a
> Vec (or its variants, like SmallVec or ArrayVec), and instead of using
> pointers to link to children, indices into the storage are used instead"

### TODO:
uses for / methods of octrees other than nearest neighbour search:

- **point membership:**  
simplest one is "does point x exist in the point set?" i.e. no priority queue or octant distances needed, just go to the target leaf and check its point-set.

- **ray intersection:**  
    https://daeken.svbtle.com/a-stupidly-simple-fast-octree-traversal-for-ray-intersection
    http://bertolami.com/files/octrees.pdf  
    Note: I think for both of these the tree stores triangles or polygons rather than points... or possibly it treats the octants themselves as the polygons, i.e. bounding boxes?

    a ray is _"...best defined in parameterized form as a point (X0, Y0, Z0) and a direction vector (Dx, Dy, Dz)"_
    
    seems like the line joining those is like a 'unit length', you can keep adding D to the line to extend it.  
    https://en.wikipedia.org/wiki/Ray_casting

- **collision detection:**  
    more complicated... find points or polygons in the tree which are inside or intersect with the given bounding box (or other polygon)  
    https://www.gamedev.net/tutorials/programming/general-and-gameplay-programming/introduction-to-octrees-r3529/  
    ...lists four kinds:
    - Frustum intersections (i.e. camera view field)
    - Ray intersections (as above)
    - Bounding Box intersections
    - Bounding Sphere Intersections

- **update value of a point:**  
    i.e. you have a scene where things are moving, either by mutation or
    replacement. It will have to move octants, and prev one may now be empty.

- **"geometric query problems"**  
    https://en.wikipedia.org/wiki/Computational_geometry#Geometric_query_problems

- **"poisson hardcode process"**  
    https://www.mathematik.uni-ulm.de/stochastik/aktuelles/sh06/sh_schmidt.pdf

    Here is an interesting one. If you sample points from a uniform distribution the result often looks surprisingly 'clumpy', because that's what true randomness looks like (especially with fewer points)

    It uses a "Poisson process" to randomly place points, then:

    > Cancel all those points whose distance to their nearest neighbor is smaller than some R > 0

    The result is something that looks more visually uniform.

See e.g.  
https://hackage.haskell.org/package/Octree-0.6.0.1/docs/Data-Octree.html  
https://github.com/BioHaskell/octree/blob/master/Data/Octree/Internal.hs

- is generic on some opaque 'payload' that is stored alongside the point  
  ...is that useful? seems likely to spoil memory usage
- works without specifying any root size
- octant distance calcs looks similar... but different?
    - reformulated as a series of `>= 0` checks instead of `<=` size checks
    - nice
    - had to [rewrite this in OCaml](tests/hask.ml) to understand what's going on
- implementation does not seem to use pre-chosen depth, instead points are added to a leaf until it hits a limit (16) and then leaf is split adding a level  
...is this performance-tuned? i.e. it's as cheap to brute-force 16 points as to do 8 octants and then down a level?
- Nodes only need a 'split point' V3 attribute... this condenses size and offset into one - I guess we don't need outer boundaries since we already split down from parent node. Our 'origin' is the parent's split point. Is theirs the same?
- in case of creation from list: root origin is chosen by 'mass centre' of the whole list of points (rather than specified as an arg)

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

    > "Class of data structures that can be traversed from left to right, performing an action on each element."
    
    this sounds a lot like map or fold, but the "actions" are either Applicative or Monad ... not quite sure what that means in practice though

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

- https://github.com/thierry-martinez/traverse OCaml

- https://mattwindsor91.github.io/travesty/travesty/Travesty/Traversable/index.html OCaml

- https://blog.shaynefletcher.org/2017/05/more-type-classes-in-ocaml.html  
    various including *Applicative* and *Traversable*
    
    > "It may be described as a generalization of the iterator pattern"
    
    > "traverse in this context is a function that allows us to map each element of a list to an optional value where the computation produces a list with all values collected, only in case every element was successfully mapped to a Some value."
    
    i.e. filter_map

- https://discuss.ocaml.org/t/notes-from-compose-2017/240/6  
    shows trivial-looking implementation of *Applicative* and *Traversable*

- It's probably a https://wiki.haskell.org/Monoid too?

- https://apfelmus.nfshost.com/articles/monoid-fingertree.html  
    > "binary search on monoids [is a] powerful tool to construct about any fancy data structure with logarithmic access time that you can imagine"

- https://academickids.com/encyclopedia/index.php/Semilattice  
    Seems to basically be about ordered sets.  
    It's a narrowing of *Semigroup* (which provides `empty` `append` and `concat`)  
    Squinting at various Haskell libs it seems to imply operations like:
    - set union, or `merge`
    - `min`, `max`, `any`, `all`, `lt`, `gt`
    
    > "Any tree structure (with the root as the least element) is a meet-semilattice."

    Wikipedia words it a bit more precisely:

    > "Any single-rooted tree (with the single root as the least element) of height `≦ ⍵` is a (generally unbounded) meet-semilattice. Consider for example the set of finite words over some alphabet, ordered by the prefix order. It has a least element (the empty word), which is an annihilator element of the meet operation, but no greatest (identity) element."
    
    at the same time...  
    https://www.reddit.com/r/compsci/comments/55rzyk/comment/d8d9os9/

    > "logical AND operator is a semilattice because it obeys three useful laws"
    
    (associativity, commutativity, idempotency)
    
    it's weird that a data structure and a logical operator can both be this thing.
    
    Is our Octree a meet-semilattice? i.e. is there some associative, commutative,
    idempotent operator we can use to compare nodes where the root node will always
    compare as 'least'?  Maybe only the Nodes and not the Points?
    
    > "...lattice theory's root as a tool to explain how to derive order relations (akin to <= and >=) on arbitrary structures"

- https://hacklewayne.com/one-recursion-for-all-catamorphism-step-by-step

    My eyes glaze over a bit, until... 

    > Of course we realise `showTreeR4` needs to be renamed to fit its now generic purpose. Indeed, it is well-known as `cata`, short for catamorphism - collapsing a structure into a single value.

    > `cata` works for any recursive type, for example, `Either`

    > There are other functions that work on recursive data structures such as `ana`, `apo`, `para`, `zygo`... This family of functions are otherwise referred to as, behold, "recursion schemes".

    > The `Rec` type [used throughout the article so far] is a synonym to the famous `Fix` type.

    https://hackage.haskell.org/package/data-fix-0.3.2/docs/Data-Fix.html

    > Type `f` should be a `Functor` if you want to use simple recursion schemes or `Traversable` if you want to use monadic recursion schemes. This style allows you to express recursive functions in non-recursive manner. You can imagine that a non-recursive function holds values of the previous iteration.

    Possibly this is related to the way my `nearest` method has an inner recursive function?

    It also gives a clue that maybe _Traversable_ is just "monadic Foldable"?

    https://wiki.haskell.org/Catamorphisms

    > Catamorphisms are generalizations of the concept of a fold in functional programming. A catamorphism deconstructs a data structure with an F-algebra for its underlying functor.

    Well, I get the first bit at least.

    > Due to this choice of notation, a catamorphism is sometimes called a banana and the (|.|) notation is sometimes referred to as banana brackets.

    🍌, got it.

    https://thealmarty.com/2018/10/16/programming-with-bananas-in-ocaml/

    > By bananas, I mean banana brackets as in the famous paper ["Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire" by Meijer et al.](http://maartenfokkinga.github.io/utwente/mmf91m.pdf)  In this post I only focus on bananas, also called **catamorphisms**.

    > Meijer et al. show that we can treat recursions as separate higher order functions known as **recursion schemes**.  Catamorphisms is one type of recursion schemes.

    Part 1: it's just `fold_left` / `fold_right` ([good explanation of the difference between l/r fold](https://stackoverflow.com/a/1446478/202168))

    https://thealmarty.com/2018/10/23/programming-with-bananas-in-haskell-versus-ocaml/

    Needing a base case for the fold breaks polymorphism. Haskell has ways around that.

    https://thealmarty.com/2018/10/30/folding-nonempty-lists-in-ocaml-and-haskell/

    For non-empty structure we can get round it too, by using the `hd` as the base case.

    http://maartenfokkinga.github.io/utwente/mmf91m.pdf

    - anything that can be implemented by a fold is a **catamorphism**

    - `unfold` is an **anamorphism** ...we can recognise this from `Seq.unfold` in OCaml

        > "Where fold abstracts a loop that consumes data, unfold abstracts a loop that produces data"

        https://discuss.ocaml.org/t/how-to-unfold-unfold-what-is-a-good-source/2039/4?u=anentropic

    - **hylomorphism** 🤷‍♂️

        > A hylomorphism corresponds to the composition of an anamorphism that builds the call-tree as an explicit data structure and a catamorphism that reduces this data object into the required value.

        > "An archetypical hylomorphism is the factorial function"

    - **paramorphism** 🤷‍♂️ 🤷‍♂️ 🤷‍♂️

    http://wide.land/hop/generalized_fold.html OCaml

    > This technique constructs something called a catamorphism, aka a generalized fold operation. To learn more about catamorphisms, take a course on category theory, such as CS 6117.

    👌

    http://wide.land/hop/fold_trees.html OCaml

    > We can then use foldtree to implement some of the tree functions we've previously seen:

    - `size`
    - `depth`
    - `preorder`

    https://www.geeksforgeeks.org/tree-traversals-inorder-preorder-and-postorder/

    > Unlike linear data structures (Array, Linked List, Queues, Stacks, etc) which have only one logical way to traverse them, trees can be traversed in different ways

    So our `points` method will currently be returning one of these ("preorder" I think)

    Returning to the mystery list of Haskell methods:

    `cata`, `ana`, `apo`, `para`, `zygo`

    I guess these are all "morphisms"

    https://stackoverflow.com/questions/36851766/histomorphisms-zygomorphisms-and-futumorphisms-specialised-to-lists  
    ...has some answers, as well as naming some more morphisms: `mutu` and `futu`

    I think basically all of these things are only relevent in context of Haskell typeclasses?

    Once specialised to concrete types they just become familiar fold etc?

    https://blog.ploeh.dk/2019/04/29/catamorphisms/

    > In category theory, a morphism is basically just an arrow that points from one object to another. Think of it as a function.

    > For some data structures, such as Boolean values, or Peano numbers, the catamorphism is all there is; no fold exists. For other data structures, such as Maybe or collections, the catamorphism and the fold coincide. Still other data structures, such as Either and trees, support folding, but the fold is based on the catamorphism. For those types, there are operations you can do with the catamorphism that are impossible to implement with the fold function. One example is that a tree's catamorphism enables you to count its leaves; you can't do that with its fold function.

    🤔 ...I'm pretty sure I could count leaves with a fold?

    https://blog.ploeh.dk/2019/06/10/tree-catamorphism/

    > ...This, hopefully, illustrates that the catamorphism is more capable, and that the fold is just a (list-biased) specialisation.

    > The catamorphism for a tree is just a single function, which is recursively evaluated. It enables you to translate, traverse, and reduce trees in many interesting ways.
    
    Now I am curious to define the catamorphism of the octree...


- https://cs3110.github.io/textbook/chapters/hop/beyond_lists.html

    (in OCaml)

    Shows generalised `map_tree`, `fold_tree` and `filter_tree`, where:

    ```ocaml
    type 'a tree =
      | Leaf
      | Node of 'a * 'a tree * 'a tree
    ```

    (FWIW I think this `Node` definition is specifically a _binary_ tree)

    Discusses how `filter_tree` must discard children when a node is filtered out.

    So our `nearest` is like a filter + fold, but at the same time?

    No exactly though, it's neither. A fold should visit every element (?) And a filter does not reduce - we are not doing a boolean test.
    
    We are kind of folding whole branches of the tree, but also deciding in that fold whether to recurse.

    But the end result is same as if we visited every element.

