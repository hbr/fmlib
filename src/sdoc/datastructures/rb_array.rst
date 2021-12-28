********************************************************************************
Radix Balanced Array
********************************************************************************


As opposed to lists, arrays allow random access. You can get any element of an
array in constant time. Lists need linear time if you want to access the
``i``\ th element of the list.

However inserting elements to an array is expensive. It requires to allocate a
new array which has room for the additional element. Then you have to copy the
original array into the new array and add the additional element to the array.

Inserting elements into lists is a cheap operation as long as you insert the new
element to the front of the list.

Radix balanced arrays (or trees) are a good compromise. They offer fast random
access, fast appending of new elements to the end of the array and fast removal
of elements from the end of the array. They are based on the idea that a long
array can be split into chunks which fit into the cache line of modern
computers. As long as the array segments fit into a cache line, inserting is a
relatively cheap operation. If we organize the chunks into a radix tree, then
random access to specific elements needs only time proportional to the height of
the tree which is very small even for very long arrays.



Basic Idea
================================================================================

We work with a branching factor :math:`B` which is a power of two. The exponent
of the branching factor is :math:`b` such that :math:`B = 2^b`. On current
microprocessors the size of a cache line is 32 machine words or more. Therefore
:math:`b = 5` and :math:`B = 2^5 = 32` are good choices. In order to illustrate
the radix balanced arrays and keep the graphics reasonably sized we use a
branching factor of 2 in the examples.

We use arrays of size :math:`B` to store the elements of the whole structure. If
we want to store 5 elements with branching factor 2 we need 3 arrays to store
the chunks



.. code-block:: none

          chunk₀     chunk₁   chunk₂
        +---+---+  +---+---+  +---+
        | e₀| e₁|  | e₂| e₃|  | e₄|
        +---+---+  +---+---+  +---+
          0   1      2   3      4


The complete tree to store 5 elements looks like

.. code-block:: none

                        size 5
                        +---+---+
                        |   |   |               level 2
                        +---+---+
                         /     \
               size 4   /       \size 1
               +---+---+         +---+
               |   |   |         |   |          level 1
               +---+---+         +---+
                /    \            /
               /      \          /
        +---+---+  +---+---+  +---+
        | e₀| e₁|  | e₂| e₃|  | e₄|             level 0
        +---+---+  +---+---+  +---+
          0   1      2   3      4

At level 0 we have all the leaves which store the elements. In this specific
case we have intermediate nodes at level 1 and at level 2. Each node at level
:math:`l` has a slot size of :math:`2^{l b}` and is capable to address
:math:`2^{(l+1) b}` elements. The following table illustrates the slot sizes and
maximal sizes for the branching factor 2 and the branching factor 32.

+----------+-------------+------------+-------------+------------+
|  level   | slot size   | max size   | slot size   | max size   |
|          | (B = 2)     | (B = 2)    | (B = 32)    | (B = 32)   |
+----------+-------------+------------+-------------+------------+
|  0       |         1   |        2   |         1   |       32   |
+----------+-------------+------------+-------------+------------+
|  1       |         2   |        4   |        32   |     1024   |
+----------+-------------+------------+-------------+------------+
|  2       |         4   |        8   |      1024   |    32768   |
+----------+-------------+------------+-------------+------------+
|  3       |         8   |       16   |     32768   |  1048576   |
+----------+-------------+------------+-------------+------------+

Note that with a branching factor 32 which fits well into the cache line of
modern microprocessors we are able to store more than 1 million elements in a
tree with only 3 intermediate levels.

The tree is balanced in the sense that each subtree of a tree has the same depth
and all subtrees except the rightmost are full.

Each subtree is a valid radix balanced tree. The counting of its elements starts
at zero.

If we want to find an element at index :math:`i` we first have to find the valid
slot :math:`s` in the tree. This can be done by integer division :math:`s = i /
2^{l b}`.

The relative index :math:`o` (i.e. its offset) in the subtree at slot :math:`s`
can be found by the formula :math:`o = i - 2^{l b} s`.

Integer divisions and multiplications by powers of 2 can be performed very
efficiently by using bitshifts. The indices are always non-negative integers.
Therefore logical shift does the same thing as arithmetic shift.

.. code-block:: ocaml

    let bitsize: int = 32

    let slot (i: int) (l: int): int =
        (* The slot of index [i] at level [l].

            i / 2^(l * bitsize)
         *)
        i lsr (l * bitsize)


    let offset (i: int) (s: int) (l: int): int =
        (* The offset of index [i] in slot [s] in a tree at level [l].

            i- s * 2^(l * bitsize)
         *)
        i - s lsl (l * bitsize)

A radix balanced array at level :math:`l` is full if it has :math:`2^{(l + 1)
b}` elements.

.. code-block:: ocaml

    let full_size (l: int): int =
        (* The size of a full radix balanced array at level [l]. *)
        assert (0 <= l);
        1 lsl ((l + 1) * bitsize)



Data Structure
================================================================================

We use an algebraic data type to define the type of a radix balanced tree. The
leaf node is just an array of the element type. An intermediate node is an array
whose elements are other radix balanced trees and has information about its
level and its total number of elements.

.. code-block:: ocaml

    type 'a t =
        | Leaf of
            'a array
        | Node of {
            size:  int; (* Total number of elements, including subtrees. *)
            level: int;
            nodes: 'a t array}

Since the total number of elements is available in all intermediate nodes, the
function to calculate the number is elements i.e. the length of the radix
balanced array is straightforward and doesn't need any recursion.

.. code-block:: ocaml

    let length: 'a t -> int =
        (* The length of the radix balanced array. *)
        function
        | Leaf arr ->
            Array.length arr
        | Node node ->
            node.size


The functions to compute the level, the fullness and the emptyness of a radix
balanced array are straightforward as well.

.. code-block:: ocaml

    let level: 'a t -> int = function
        | Leaf _ ->
            0
        | Node node ->
            node.level

    let is_full: 'a t -> bool = function
        | Leaf arr ->
            Array.length arr = full_size 0
        | Node node ->
            node.size = full_size node.level

    let is_empty (t: 'a t): bool =
        length t = 0

    let has_some (t: 'a t): bool =
        length t > 0


Invariant
================================================================================

The following invariant is maintained by all operations where :math:`B` is the
branching factor:

- Each leaf node has at most :math:`B` elements.

- Each leaf node which is not the root node has at least one element.

- Each interior node has at most :math:`B` children.

- Each interior node has at least one child.

- If the root node is an interior node it has at least two children (a root node
  with only one child makes no sense and can be removed and the child can be
  used as the root).

- All children of an interior node except the last child are full.

- All children of an interior node have the same level and the level of the
  interior node is one higher than the level of its children.

- The size of an interior node is the sum of the size of the children.




Element Retrieval
================================================================================

The following function retrieves the element at a certain index :math:`i` i.e.
it implements the random access.

.. code-block:: ocaml

    let rec element (i: int) (t: 'a t): 'a =
        (* The element at index [i] in the radix balanced array [t]. *)
        assert (0 <= i);
        assert (i < length t);
        match t with
        | Leaf arr ->
            arr.(i)
        | Node node ->
            let s = slot i node.level in
            let o = offset i s node.level in
            element o node.nodes.(s)


Note that :math:`i` is the relative index within the subtree. Before going down
one level we have to find the slot and the relative offset within the slot.

In a nonempty array the first and last element of the array can be retrieved by
using the function ``element``. However more efficient functions can be defined
which do not need any slot and offset computations. The first element of an
intermediate node is always in the first child (i.e. first slot) and the last
element of an intermediate node is always in the last child (i.e. last slot).

.. code-block:: ocaml

    let first (t: 'a t): 'a =
        (* The first element of the non empty radix balanced array [t]. *)
        assert (has_some t);
        let rec fst = function
            | Leaf arr ->
                Array.first arr
            | Node node ->
                fst (Array.first node.nodes)
        in
        fst t


    let last (t: 'a t): 'a =
        (* The last element of the non empty radix balanced array [t]. *)
        assert (has_some t);
        let rec fst = function
            | Leaf arr ->
                Array.last arr
            | Node node ->
                fst (Array.last node.nodes)
        in
        fst t



Element Replacement
================================================================================


Replacing an element within the radix balanced array affects only nodes on the
path from the root to the corresponding leaf. In the leaf the element has to be
replaced by the new element. In each intermediate node the child at the
corresponding slot has to be replaced by the new child.

.. code-block:: ocaml

    let rec replace (i: int) (e: 'a) (t: 'a t): 'a t =
        (* Replace the element at index [i] by the element [e] within the radix
           balanced array [t]. *)
        assert (0 <= i);
        assert (i < length t);
        match t with
        | Leaf arr ->
            Leaf (Array.replace i e arr)
        | Node node ->
            let s = slot i node.level in
            let o = offset i s node.level in
            Node
                {node with
                    nodes =
                        Array.replace
                            s
                            (replace o e node.nodes.(s))
                            node.nodes
                }


Each replacement within an elementary array requires an array copy. But remember
that the elementary arrays always fit into a cache line such that the operations
are very fast.



Element Insertion at the Rear End
================================================================================

Radix balanced arrays are *balanced* and *packed* which means that all subtrees
have the same height and are full except the rightmost subtrees. Therefore cheap
insertion is possible only at the rear end of the array. It is possible to relax
the invariant like it is done in *radix relaxed balanced trees*. However this
has a cost for the runtime performance. The implementation described here has
no such relaxation because it tries to be as efficient as possible and keep all
subtrees packed and fully balanced.


Remember that a radix balanced tree at the level :math:`l` is full if it has
:math:`2^{(l+1) b}` elements.

We want to insert an element :math:`e` at the rear end of the tree :math:`t`

If the balanced tree is full, it is not possible to insert an element into the
tree. The only thing we can do is to construct a new tree at the same level with
only one element.

.. code-block:: none

    +---+
    |   |               level 3
    +---+
      |
    +---+
    |   |               level 2
    +---+
      |
    +---+
    |   |               level 1
    +---+
      |
    +---+
    | e |               level 0
    +---+


.. code-block:: ocaml

    let rec singleton_tree (lev: int) (e: 'a): 'a t =
        (* Construct tree at level [lev] with the element [e]. *)
        if lev = 0 then
            Leaf [| e |]
        else
            Node {
                size = 1;
                level = lev;
                nodes = [| singleton_tree (lev - 1) e |]
            }


If the tree :math:`t` has a parent, then it is the last child of the parent.
Since :math:`t` is full and the parent is not full, we can append the singleton
tree to the nodes of the parent.

If the tree :math:`t` is the root, then we need a new root node with :math:`t`
as the first child and the singleton tree as the second child.

Since we have constructed the singleton tree at the same level as the tree
:math:`t`, the new tree is still balanced.

Insertion into a not full tree can be done by the following recursive function:

.. code-block:: ocaml

    let rec push_not_full (e: 'a) (t: 'a t): 'a t =
        (* Append the element [e] at the rear end of the radix balanced array
           [t] which is not full. *)
        assert (not (is_full t));
        match t with
        | Leaf arr ->
            Leaf (Array.push e arr)

        | Node node ->
            let slot = Array.length node.nodes - 1
            assert (0 <= slot);
            in
            let nodes =
                if is_full node.nodes.slot then
                    Array.push
                        (singleton_tree (node.level - 1))
                        node.nodes
                else
                    Array.replace
                        slot
                        (push_not_full e node.nodes.(slot))
                        node.nodes
            in
            Node
                {node with nodes; size = node.size + 1}

Finally we get the complete insertion function.

.. code-block:: ocaml

    let push (e: 'a) (t: 'a t): 'a t =
        (* Append the element [e] at the rear end of the radix balanced array
           [t]. *)
        let lev = level t
        and len = length t
        in
        if len = full_size lev then
            Node {
                size  = len + 1;
                level = lev + 1;
                nodes = [| t; singleton_tree lev e|]
            }
        else
            push_not_full e t



Element Deletion at the Rear End
================================================================================

The structure of the radix balanced tree allows fast removal of elements on the
rear end. This is possible only if the array is not empty. We want to write a
function with the following signature

.. code-block:: ocaml

    val pop (t:'a t): 'a * 'a t
    (* Pop the last element off the array [t] and return it together with
       the array where the last element has been removed.

       Precondition: [t] must not be empty i.e. [has_some t]
    *)

The basic algorithm is not complicated.

If the tree is a leaf node we just remove the last element from the non empty
array.

If the tree is an interior node, then we take the last child and remove
recursively the last element from the child and replace the last child by the
child where the last element has been removed. However we have to consider the
following corner case:

The last child has only one element:
    In that case the child disappears completely. I.e. the new interior node has
    one child less than the old interior node. We have to distinguish two cases:

    The parent node is the root node:
        The root node has at least two children. Since the last child has only
        one element, we remove the last child completely. Since there remains
        only one child in the root node, we can replace the root node by its
        first child.

    The parent node is not the root node:
        The parent node has at least two children. Otherwise it would have only
        one element and would have been removed completely. Since the last child
        has only one element, we remove the last child completely.

The following helper function removes the last element from a nonempty tree.

.. code-block:: ocaml

    let rec pop_aux (is_root: bool) (t: 'a t): 'a * 'a t =
        (* Remove the last element from a nonempty tree. *)
        assert (has_some t);
        match t with
        | Leaf arr ->
            Array.(last arr, Leaf (remove_last arr))
        | Node node ->
            let j = Array.length node.nodes - 1 in
            assert (0 <= j);
            let child = node.nodes.(j) in
            let len   = length child in
            if is_root && j = 1 && len = 1 then
                (* Last child of the root node has only one element. *)
                last child,
                node.nodes.(0)
            else
                let e, nodes =
                    if len = 1 then
                        (* Last child has only one element. *)
                        last child,
                        Array.remove_last node.nodes
                    else
                        (* Normal case. *)
                        let e, child = pop_aux false child in
                        e,
                        Array.replace j child node.nodes
                in
                e,
                Node {
                    node with
                    size = node.size - 1;
                    nodes
                }


With the helper function is straightforward. We can write two versions of the
removal function. The first assumes that the tree is not empty. The second one
works on empty trees as well (returning an option).

.. code-block:: ocaml

    let pop (t: 'a t): 'a * 'a t =
        assert (has_some t);
        pop_aux true t

    let pop_opt (t: 'a t): ('a * 'a t) option =
        if is_empty t then
            None
        else
            Some (pop_aux true t)
