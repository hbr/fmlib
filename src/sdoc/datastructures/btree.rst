************************************************************
B Tree
************************************************************



General
============================================================


A B-tree has interior nodes and leaf nodes. The leaf nodes have no children. The
interior nodes have children. The root node is either an interior node (if it
has children) or a leaf node (if it doesn't have children).

Each node carries keys (or key value pairs). The keys are sorted. Each key
separates two children in the interior nodes. The child to the left of a key has
only keys strictly lower than the key and the child to the right of a key has
only keys strictly greater than the key.

Each interior node has one child more than the number of keys. The maximum
number ``m`` of children in a B tree is called its *order*. The minimal order of
a B tree is 3.

.. code-block:: none


                Example: B-tree of order 3

                    +---+
                    | 4 |                             root node / interior node
                    +---+
                 /         \
        +---+                +---+---+
        | 2 |                | 7 | 9 |                interior nodes
        +---+                +---+---+
        |   |               /    |    \
    +---+   +---+   +---+---+  +---+   +---+
    | 1 |   | 3 |   | 5 | 6 |  | 8 |   | 10|          leaf nodes
    +---+   +---+   +---+---+  +---+   +---+



A B-tree of order ``m`` satisfies the following invariant:


- Every node has at most ``m - 1`` keys and ``m`` children in case of an
  interior node.

- Every node (except the root node) has at least ``ceil (m / 2) - 1`` keys and
  ``ceil (m / 2)`` children in case of an iterior node.

- The tree is balanced in the sense that all leaves appear at the same level.

- The tree is sorted in the sense that all keys of the child to the left of a
  key are strictly smaller than the key and all keys of the child to the right
  of a key are strictly greater than the key.


Note that ``m / 2`` is not an integral value if ``m`` is an odd number.
Therefore ``ceil (m / 2)`` rounds the value up to its nearest integral number.
Example: If the order is 3, then ``3 / 2 = 1.5`` and ``ceil (3 / 2) = 2``. I.e.
a B tree of order 3 has at least 2 children and 1 key and at most 3 children and
2 keys.


+---------+--------------+-------------+
|  order  |   keys       |  children   |
+---------+--------------+-------------+
|    3    |   1 - 2      |    2 - 3    |
+---------+--------------+-------------+
|    4    |   1 - 3      |    2 - 4    |
+---------+--------------+-------------+
|    5    |   2 - 4      |    3 - 5    |
+---------+--------------+-------------+
|    6    |   2 - 5      |    3 - 6    |
+---------+--------------+-------------+
|   32    |  15 - 31     |   16 - 32   |
+---------+--------------+-------------+


According to the invariant each node except the root node has at least the
minimal number of keys. Any interior node except the root node has at least the
miminal number of children.



The keys (or key value pairs) and the pointers to the children are usually
stored in arrays. If we choose a power of two for the order which represents the
caches size of a processor (e.g. 32), then the arrays can be stored within a
cache line which speeds up insertion an deletion and results in better data
locality.


B trees can be used to implement finite maps i.e. to store and retrieve key
value pairs or finite sets i.e. to store keys. The keys have to be sortable. In
this section we describe the the implementation of finite map via B trees.

In ocaml we use a module functor of the following form:


.. code-block:: ocaml

    module Map (Key: SORTABLE) =
    struct

        let order = ...                             (* usually 32 for cache efficiency *)

        let odd_order: bool =                       (* is the order odd? *)
            assert (3 <= order);                    (* minimal order 3 is a must *)
            order / 2 * 2 < order

        let max_keys: int = order - 1               (* the maximal number of keys *)

        let min_keys: int =                         (* the minimal number of keys *)
            if odd_order then
                (order - 1) / 2
            else
                order / 2 - 1

        type 'a pairs = (Key.t * 'a) array

        type 'a t =
            | Leaf of 'a pairs                 (* key value pairs *)
            | Node of 'a pairs * 'a t array    (* key value pairs + children *)

        ...
        (* Search, insert and delete see below ... *)
    end

with the module type ``SORTABLE`` which is defined in the module ``Fmlib_std.Ìnterfaces``.

.. code-block:: ocaml

    module type SORTABLE = sig
        type t

        val compare: t -> t -> int
        (** [compare a b]

            compare a b < 0         if and only if a < b
            compare a b = 0         if and only if a = b
            compare a b > 0         if and only if a > b
        *)


A leaf consists of an array of keys (or key value pairs)

.. code-block:: none

       0    1    2                              len
    +----+----+------------------------------+
    | k0 | k1 | ...                          |
    +----+----+------------------------------+


where ``k0``, ``k1``, ... ``k(len-1)`` are the keys of the leaf.


An interior node consists of an array of keys and an array of children where the
array of children has one more element than the array of keys.

.. code-block:: none

       0    1    2                              len
    +----+----+------------------------------+
    | k0 | k1 | ...                          |
    +----+----+------------------------------+
    |    |    |                              |
    c0   c1   c2                             c(len)



``c0``, ``c1``, ``c2``, ... ``c(len)`` are the children with the property
that all keys in the child ``c1`` are strictly less than the key ``k1`` and all
the keys in the child ``c2`` are strictly greater than the key ``k1``.




Search
============================================================

Since the keys in a B tree are sorted we can search within a B tree using binary
search. Let's assume we have a binary search function of the form

.. code-block:: ocaml

    let bsearch (key: Key.t) (arr: 'a pairs): int * bool =
        ...

We can search within the leaf and the interior nodes the corresponding array of
key value pairs for the position of the key ``key``. The function returns the
position and an exact flag. The exact flag indicates if an exact match has been
found. If the flag is not set, then the search key is strictly smaller than the
key at the position.  If the length is returned as the position, then we know
that all keys in the array are strictly smaller than the search key.

If no exact match can be found in a leaf node, then the key is not in the leaf.

If no exact match can be found in an interior node, then the search key is not
in the interior node. However it can be in the child at the corresponding
position. Note that the array of the children has always one more element than
the array of key value pairs. Therefore there is a valid child at the position
``length pairs``.

The search algorithm can be implemented by a straightforward recursive function

.. code-block:: ocaml

    let rec find_opt (key: Key.t) (map: 'a t): 'a option =
        match map with
        | Leaf pairs ->
            let i, exact = bsearch key pairs in
            if exact then
                Some (snd pairs.(i))
            else
                None

        | Node (pairs, children) ->
            let i, exact = bsearch key pairs in
            if exact then
                Some (snd pairs.(i))
            else
                find_opt key children.(i)




Insertion
============================================================

All nodes have at most ``m - 1`` keys and ``m`` children where ``m`` is the
order of the B tree. A node which has exactly ``m - 1`` keys (and ``m`` children
in case of an interior node) is full.

Insertion always starts in a leaf node. If the leaf node is full, then the
insertion causes an overflow. The overflow condition might pop up to the root.
In that situation the height of the tree grows.

An insertion of a key value pair in a tree which has already a key value pair
with the same key causes a nondestructive overwrite of the old value by the new value. The structure of the B tree remains the same, just the value of the corresponding key value pair will be updated.

A proper insertion of a new key value pair starts by searching the leaf node and
the position in the leaf node where to insert the new pair. The result of the
insertion is described by the data type

.. code-block:: ocaml

    type 'a insert =
        | Normal_insert of 'a t
        | Split_insert of 'a t * (Key.t * 'a) * 'a t

I.e. we insert the new pair into a leaf node and either return a new leaf node
if there is enough room in the node or a splitted leaf node with a new key value
pair which has to be inserted into the corresponding parent or a splitted leaf
node which consist of a left leaf node, a popup key value pair and a right leaf
node.

The insertion into the parent can either end in a normal insert or in a split
insert.

During insertion the invariant is maintained that the popup key separates all
the key value pairs of the left tree from the key value pairs of the right tree.

The basic insertion function looks like

.. code-block:: ocaml

    let add (key: Key.t) (value: 'a) (map: 'a t): 'a t =
        match add_aux key value map with
        | Normal_insert map ->
            map
        | Split_insert (left, popup_key, right) ->
            (* tree grows at the root *)
            Node ([| popup_key |], [| left; right |]


If the splitting reaches the root, then a new root is created with one key value
pair (the popup pair) and two children.

The basic insertion function uses the auxiliary function ``add_aux`` which
implements the recursive algorithm.


.. code-block:: ocaml

    let rec add_aux (key: Key.t) (value: 'a) (map: 'a t): 'a insert =
        match map with
        | Leaf pairs ->
            add_in_leaf key value pairs

        | Node (pairs, children) ->
            let i, exact = bsearch key pairs in
            if exact then
                (* An exact match has been found. Therefore update the value. *)
                let pairs = Array.replace i (key,value) pairs in
                Normal_insert (Node (pairs, children))
            else
                (** Add the key value pair into the [i]th child. *)
                match add_aux key value children.(i) with
                | Normal_insert child ->
                    let children = Array.replace i child children in
                    Normal_insert (Node (pairs, children))
                | Split_insert (u, y, v) ->
                    add_in_node i u y v pairs children

The function ``add_aux`` uses the two helper functions ``add_in_leaf`` and
``add_in_node`` to insert the key value pair either into a leaf node or an
interior node.



Insertion into a leaf node
------------------------------------------------------------

The insertion into a leaf node is easy, if the leaf is not full. In case of
overflow we have to distinguish several cases.

In the following we assume that we want to insert a key value pair ``y`` at
position ``i`` into a full leaf node. As a result we want to get a triple
``(left, popup_key, right)`` where ``left`` is the left tree after the split,
``right`` is the right key after the split and the key in the key value pair
``popup_key`` separates the keys in ``left`` from the keys in ``right``.

Now we analyze the different cases.

Overflow, odd order:
    ``m = 2 * k + 1``, i.e. we have ``2 * k`` keys and the array of the key
    value pairs consists of two subarrays of size ``k``. At the right end of the
    left subarray there is a key ``s1`` and at the left end of the right
    subarray there is a key ``s2``.

    We have to distinguish the cases ``i = k``, ``i < k`` and ``i > k``.

    ``i = k``:
        In that case we have ``s1 < y < s2``. We split the array into the two
        subarrays of size ``k`` and use ``y`` as the popup key.

        .. code-block:: none


            before:
                                    i
                                    k                      2k
            +-----------------+---+---+------------------+
            |                 | s1| s2|                  |
            +-----------------+---+---+------------------+


            after:
                                  +---+
                                  | y |
                                  +---+
                                  |   |
            +-----------------+---+   +---+------------------+
            |                 | s1|   | s2|                  |
            +-----------------+---+   +---+------------------+



    ``i < k``:
        In that case we have ``y < s1`` and we have to insert ``y`` into the
        left subarray and use ``s1`` as the popup key.

        .. code-block:: none


            before:
                     i              k                      2k
            +------+---+------+---+---+------------------+
            |      | z |      | s1| s2|                  |
            +------+---+------+---+---+------------------+


            after:
                                  +---+
                                  | s1|
                                  +---+
                     i            |   |
            +------+---+---+------+   +---+------------------+
            |      | y | z |      |   | s2|                  |
            +------+---+---+------+   +---+------------------+


    ``i > k``:
        In that case we have ``s2 < y`` and we have to insert ``y`` into the
        right subarray and use ``s2`` as the popup key.

        .. code-block:: none


            before:
                                    k        i             2k
            +-----------------+---+---+----+---+---------+
            |                 | s1| s2|    | z |         |
            +-----------------+---+---+----+---+---------+


            after:
                                  +---+
                                  | s2|
                                  +---+
                                  |   |
            +-----------------+---+   +-----+---+---+--------+
            |                 | s1|   |     | y | z |        |
            +-----------------+---+   +-----+---+---+--------+


Overflow, even order:
    ``m = 2 * k``, i.e. we have ``2 * k - 1`` key value pairs and the array of
    key value pairs consists of two subarrays of size ``k - 1`` with a single
    separator pair ``s`` which separates the two subarrays.

    We have to distinguish the cases ``i < k`` and ``i >= k``. In both cases ``s``
    can be used as the popup key. The insertion of ``y`` happens either in the
    left or in the right subarray.

    ``i < k``:

        .. code-block:: none

            before:
                        i            k
            +---------+---+----+---+------------------+
            |         | z |    | s |                  |
            +---------+---+----+---+------------------+


            after:

                                    +---+
                                    | s |
                                    +---+
                        i           |   |
            +---------+---+---+-----+   +------------------+
            |         | y | z |     |   |                  |
            +---------+---+---+-----+   +------------------+



    ``i >= k``:

        .. code-block:: none


            before:
                                     k    i
            +------------------+---+----+---+---------+
            |                  | s |    | z |         |
            +------------------+---+----+---+---------+


            after:

                               +---+
                               | s |
                               +---+
                               |   |
            +------------------+   +-----+---+---+---------+
            |                  |   |     | y | z |         |
            +------------------+   +-----+---+---+---------+






The following function does the insertion into a leaf node.

.. code-block:: ocaml

    let add_in_leaf (key: Key.t) (value: 'a) (pairs: 'a pairs): 'a insert =
        let len = Array.length pairs in
        let i, exact = bsearch key pairs in
        if exact then
            Normal_insert (Leaf (Array.replace i (key, value) pairs))

        else if len < max_keys then
            (* Leaf is not full. *)
            Normal_insert (Leaf (Array.insert i (key, value) pairs))

        else
            (* Leaf is full *)
            let insert_subarray = insert_subarray pairs i (key, value)
            and k = order / 2
            in
            if odd_order then
                if i = k then
                    let left  = subarray pairs 0 k
                    and right = subarray pairs k len
                    in
                    Split_insert (Leaf left, (key, value), Leaf right)
                else if i < k then
                    let left  = insert_subarray 0 (k - 1)
                    and right = subarray pairs k len
                    in
                    Split_insert (Leaf left, pairs.(k - 1), Leaf right)
                else
                    let left  = subarray pairs 0 k
                    and right = insert_subarray (k + 1) len
                    in
                    Split_insert (Leaf left, pairs.(k), Leaf right)
            else begin
                (* even order *)
                if i < k then
                    let left  = insert_subarray 0 (k - 1)
                    and right = subarray pairs k len
                    in
                    Split_insert (Leaf left, pairs.(k - 1), Leaf right)
                else
                    let left  = subarray pairs 0 (k - 1)
                    and right = insert_subarray k len
                    in
                    Split_insert (Leaf left, pairs.(k - 1), Leaf right)
            end

Using the two helper functions ``subarray`` and ``insert_subarray``.

.. code-block:: ocaml

    subarray (arr: 'a array) (start: int) (beyond: int): 'a t =
        (* The subarray of [arr] starting at [start] and ending one before [beyond]. *)
        assert (0 <= start);
        assert (start <= beyond);
        assert (beyond <= Array.length arr);
        Array.sub arr start (beyond - start)


    let insert_subarray
            (arr: 'a array) (i: int) (x: 'a) (start: int) (beyond: int)
        : 'a array
        =
        (* The subarray of [arr] starting at [start] and ending one before [beyond]
           with [x] inserted at position [i]. *)
        assert (0 <= start);
        assert (start <= i);
        assert (i <= beyond);
        assert (beyond <= Array.length arr);
        let arr2 = Array.make (beyond - start + 1) x in
        Array.blit arr start arr2 0 (i - start);
        Array.blit arr i arr2 (i - start + 1) (beyond - i);
        arr2






Insertion into an interior node
------------------------------------------------------------

In this subsection we treat the situation that a key value pair has been
inserted into the ``i`` th child ``t`` of an interior node and the insertion into
the child ``t`` caused an overflow and resulted in the triple ``(u, y, v)``
where ``u`` and ``v`` are two valid B trees separated by the popup key value
pair ``y``. Furthermore the interior node is full and must be splitted as well.

The case distinctions are the same as in the insertion into a full leaf node
with the additional complexity that the child nodes have to be treated.


Overflow, odd order:
    ``m = 2 * k + 1``. The array of the key value pairs consists of two subarray
    of equal size ``k`` with the key value pair ``s1`` ending the left subarray
    and ``s2`` beginning the right subarray.

    We have to distinguish the cases ``i = k``, ``i < k`` and ``i > k``.

    ``i = k``:
        ``s1`` is a strict lower bound of all keys in ``t`` and therefore for
        all keys in ``(u, y, v)``. ``s2`` is a strict upper bound. We split the
        interior node into the two subarrays and use ``u`` as the last child in
        the left part and ``v`` as the first child in the right part. We use
        ``y`` as the popup key.

        .. code-block:: none

            before:
                                    i
                                    k                      2k
            +-----------------+---+---+------------------+
            |                 | s1| s2|                  |
            +-----------------+---+---+------------------+
                                  |
                                  t


            after:
                                  +---+
                                  | y |
                                  +---+
                                  |   |
            +-----------------+---+   +---+------------------+
            |                 | s1|   | s2|                  |
            +-----------------+---+   +---+------------------+
                                  |   |
                                  u   v

    ``i < k``:
        In that case we have to insert the popup key ``y`` at position ``i`` of
        the key value pairs of the interior node, replace the ``i``th child
        ``t`` by ``u`` and use ``v`` as the additionally needed child.

        .. code-block:: none


            before:
                     i              k                      2k
            +------+---+------+---+---+------------------+
            |      | z |      | s1| s2|                  |
            +------+---+------+---+---+------------------+
                   |
                   t


            after:
                                  +---+
                                  | s1|
                                  +---+
                     i            |   |
            +------+---+---+------+   +---+------------------+
            |      | y | z |      |   | s2|                  |
            +------+---+---+------+   +---+------------------+
                   |   |
                   u   v


    ``i > k``:
        The same as before just with insertion into the right part of the
        interior node.

        .. code-block:: none


            before:
                                    k        i             2k
            +-----------------+---+---+----+---+---------+
            |                 | s1| s2|    | z |         |
            +-----------------+---+---+----+---+---------+
                                           |
                                           t


            after:
                                  +---+
                                  | s2|
                                  +---+
                                  |   |
            +-----------------+---+   +-----+---+---+--------+
            |                 | s1|   |     | y | z |        |
            +-----------------+---+   +-----+---+---+--------+
                                            |   |
                                            u   v


Overflow, even order:
    ``m = 2 * k``.

    We have to distinguish the cases ``i < k`` and ``i >= k``.

    ``i < k``:

        .. code-block:: none

            before:
                        i            k
            +---------+---+----+---+------------------+
            |         | z |    | s |                  |
            +---------+---+----+---+------------------+
                      |
                      t


            after:

                                    +---+
                                    | s |
                                    +---+
                        i           |   |
            +---------+---+---+-----+   +------------------+
            |         | y | z |     |   |                  |
            +---------+---+---+-----+   +------------------+
                      |   |
                      u   v




    ``i >= k``:

        .. code-block:: none


            before:
                                     k    i
            +------------------+---+----+---+---------+
            |                  | s |    | z |         |
            +------------------+---+----+---+---------+
                                        |
                                        t


            after:

                               +---+
                               | s |
                               +---+
                               |   |
            +------------------+   +-----+---+---+---------+
            |                  |   |     | y | z |         |
            +------------------+   +-----+---+---+---------+
                                         |   |
                                         u   v


The insertion function which inserts into an interior node reads like

.. code-block:: ocaml

    let add_in_node
            (i: int)
            (left: 'a t)
            (pair: Key.t * 'a)
            (right: 'a t)
            (pairs: 'a pairs)
            (children: 'a t array)
        : 'a insert
        =
        let len = Array.length pairs in
        if len < max_keys then
            let pairs = Array.insert i pair pairs
            and children = Array.insert i left children
            in
            children.(i + 1) <- right;
            Normal_insert (Node (pairs, children))
        else
            (* Node is full. *)
            let k = order / 2
            and insert_subarray = insert_subarray pairs i pair
            and split_subarray start beyond =
                split_subarray children i left right start beyond
            in
            if odd_order then
                if i = k then
                    let left_pairs     = subarray pairs    0 k
                    and left_children  = subarray children 0 (k + 1)
                    and right_pairs    = subarray pairs    k len
                    and right_children = subarray children k (len + 1)
                    in
                    left_children.(k)  <- left;
                    right_children.(0) <- right;
                    Split_insert (
                        Node (left_pairs, left_children),
                        pair,
                        Node (right_pairs, right_children))
                else if i < k then
                    let left_pairs     = insert_subarray 0 (k - 1)
                    and left_children  = split_subarray  0 k
                    and right_pairs    = subarray pairs    k len
                    and right_children = subarray children k (len + 1)
                    in
                    Split_insert (
                        Node (left_pairs, left_children),
                        pairs.(k - 1),
                        Node (right_pairs, right_children))
                else begin
                    let left_pairs     = subarray pairs    0 k
                    and left_children  = subarray children 0 (k + 1)
                    and right_pairs    = insert_subarray (k + 1) len
                    and right_children = split_subarray  (k + 1) (len + 1) in
                    Split_insert (
                        Node (left_pairs, left_children),
                        pairs.(k),
                        Node (right_pairs, right_children))
                end
            else begin
                (* even order *)
                if i < k then
                    let left_pairs     = insert_subarray 0 (k - 1)
                    and left_children  = split_subarray  0 k
                    and right_pairs    = subarray pairs    k len
                    and right_children = subarray children k (len + 1)
                    in
                    Split_insert (
                        Node (left_pairs, left_children),
                        pairs.(k - 1),
                        Node (right_pairs, right_children))
                else
                    let left_pairs     = subarray pairs    0 (k - 1)
                    and left_children  = subarray children 0 k
                    and right_pairs    = insert_subarray k len
                    and right_children = split_subarray  k (len + 1)
                    in
                    Split_insert (
                        Node (left_pairs, left_children),
                        pairs.(k - 1),
                        Node (right_pairs, right_children))
            end

with the additional helper function

.. code-block:: ocaml

    let split_subarray
            (arr: 'a array) (i: int) (x: 'a) (y: 'a) (start: int) (beyond: int)
        : 'a array
        =
        (* The subarray of [arr] starting at [start] and ending one before [beyond]
           with [x] inserted at position [i] and the original value at position
           [i] replaced by [y]. *)
        assert (i < beyond);
        let arr = insert_subarray arr i x start beyond in
        arr.(i - start + 1) <- y;
        arr








Deletion
============================================================


Basic Deletion
------------------------------------------------------------

As with insertion, an actual deletion can only be done on a leaf node.

If the to be deleted key value pair is located in an interior node, then it
cannot be deleted directly. In order to delete a key value pair in an interior
node, we have to find a direct neighbour (predecessor or successor with respect
to the order), which is always in a leaf node and delete the direct neighbour.
Since the deleted key value pair is a direct neighbour of the to be deleted key
value pair we can substitute the deleted key value pair for the key value pair
in the interior node without disturbing the order.

Furthermore we have to handle a possible underflow condition. The invariant of a
B tree requires that each leaf node or interior node which is not the root of
the tree must have a minimal number of keys (and a minimal number of children in
case of an interior node).

If a node underflows because of the deletion of a key value pair, the
missing key value pair can be pulled from the parent and the missing child from
the sibling. This can require to merge the underflowing child with its sibling
(details see below).

The reparation might cause the parent to underflow. The reparation of the
underflow condition of the parent can be done with the help of the parent of the
parent etc. The underflow condition might popup recursively to the root.

An underflow condition in the root node is no problem except the case that the
root becomes empty (i.e. no key value pairs and just one child). In that case the
height of the tree shrinks at the root and the child becomes the new root.

In order to handle deletion we use the datastructure::

    type 'a delete = {
        tree:  'a t;        (* The tree with the deleted key value pair. *)
        pair:   Key.t * 'a; (* The deleted key value pair. *)
        underflow: bool;    (* one key less than the minimal number *)
    }


The basic deletion looks like::

    let remove (key: Key.t) (map: 'a t): 'a t =
        match remove_aux key map with
        | None ->
            map
        | Some d ->
            match d.tree with
            | Node (pairs, children) when Array.is_empty pairs ->
                (* tree shrinks at the root *)
                children.(0)
            | _ ->
                d.tree

It uses an auxiliary function ``remove_aux`` which returns an optional ``'a
delete`` structure. The auxiliary function returns ``None`` if the tree does not
have a key value pair with the desired key. In case that the tree contains a key
value pair with the desired key, a tree is returned with the key value pair
deleted. The function ``remove`` has to check, if the root node is an interior
node with no key value pairs. In that case the tree shrinks in height and the
only child becomes the new root.

The function ``remove_aux`` implements the recursion. ::

    let rec remove_aux (key: Key.t) (map: 'a t): 'a delete option =
        match map with
        | Leaf pairs ->
            let i, exact = bsearch key pairs in
            if exact then
                let pair =  pairs.(i)
                and pairs = Array.remove i pairs
                and underflow = Array.length pairs <= min_keys
                in
                Some {
                    tree = Leaf pairs;
                    pair;
                    underflow
                }
            else
                None

        | Node (pairs, children) ->
            let i, exact = bsearch key pairs in
            if exact then
                let d = remove_last children.(i) in
                let pair  = pairs.(i)
                and pairs = Array.replace i d.pair pairs in
                Some (handle_delete i pair pairs children d)
            else
                Option.map
                    (fun d -> handle_delete i d.pair d pairs children)
                    (remove_aux key children.(i))


The function searches in a leaf node and in an interior node for the key value
pair which has to be deleted.

In case of an exact match, the key is deleted. In a leaf node the deletion is
straightforward. In an interior node the deletion cannot be done directly. The
last key value pair in the child to the left of the to be deleted key value pair
is deleted with the help of the function ``remove_last`` and the last key value
pair is substituted for the to be deleted key value pair. Remember that the last
key value pair in the child to the left of the to be deleted key value pair are
direct neighbours.

The function ``handle_delete`` checks for an underflow condition and does the
reparation if needed.

If the search does not find an exact match in a leaf node, then no key value
pair with the to be deleted key exists and nothing has to be done.

If the search does not find an exact match in an interior node, then the
deletion continues in the child to the left of the key.

The code of the function ``handle_delete`` is straightforward and uses the
helper function ``handle_underflow`` to handle an underflow condition. ::

    let handle_delete
            (i: int)                (* Index of the child where the deletion occurred. *)
            (pair: Key.t * 'a)      (* The deleted key value pair. *)
            (d: 'a delete)          (* The new tree with the key value pair deleted. *)
            (pairs: 'a pairs)       (* The key value pairs of the parent. *)
            (children: 'a t array)  (* The children of the parent. *)
        : 'a delete
        =
        if not d.underflow then
            {
                tree = Node (pairs, Array.replace i d.tree children);
                pair;
                underflow = false
            }
        else
            let len = Array.length pairs in
            if i < len then
                handle_underflow i true d.tree children.(i + 1) pair pairs children
            else
                let i = i - 1 in
                handle_underflow i false children.(i) d.tree pair pairs children

If the deletion in the child has not caused an underflow in the chid, the new
child is substituted for the old child. This cannot case an underflow in the
parent either.


If the deletion in the child has caused an underflow, then a sibling has to be
used to repair the underflow.

As long as the child is not the last child of the parent, there is a right
sibling. The new child and its right sibling are passed to the function
``handle_underflow`` with the indication that the underflow happened in the
first tree of the two siblings.

If the child is the last child of the parent, then we use its left sibling and
hand it over to the function ``handle_underflow``.

Since a valid B tree interior node has at least one key value pair and two
children (even the root!) there is always either a left or a right sibling.




Handling of Underflow
------------------------------------------------------------


In this section we treat the case that the deletion in one of the children of a
parent node caused an underflow and how this underflow in the child can be
repaired with the help of the parent and a sibling. We assume that the underflow
happend in the left child. The situation where the underflow happened in the
right child is symmetrical.



.. code-block:: none

                                           i
                   +---------------------+---+--------------------+
    parent node    |                     | y |                    |
                   +---------------------+---+--------------------+
                                         |   |
                         +-----------+---+   +---+-----------------+
    siblings             |           | x |   | z |                 |
                         +-----------+---+   +---+-----------------+
                                         |   |
                                         u   v

                            underflow           no underflow


In the picture it is assumed that the siblings are interior nodes and have
children. If the children are leaf nodes, then the algorithm is the same. Just
ignore the children of the siblings.

The left sibling has exactly one missing key value pair and exactly one missing child. Reason: Since both are not the root node they have at least the minimal number of keys and children before the deletion. Deletion happened in the left sibling i.e. the left sibling had the minimal number of keys and children before the deletion. Otherwise no underflow would have happened.

We have to distinguish two cases: The right sibling is not minimal of the right
sibling is minimal.

Right sibling is not minimal:
    In that case we can chop off the first child ``v`` and the first key value
    pair ``z`` from the right sibling without violating the B tree invariant.
    The key value pair ``z`` can be pushed up to the parent and key value pair
    ``y`` from the parent and the B tree ``v`` can be appended to the
    underflowing chid to repair the situation.

    .. code-block:: none

        rotation before:

                                                   i
                           +---------------------+---+--------------------+
            parent node    |                     | y |                    |
                           +---------------------+---+--------------------+
                                                 |   |
                                 +-----------+---+   +---+-----------------+
            siblings             |           | x |   | z |                 |
                                 +-----------+---+   +---+-----------------+
                                                 |   |
                                                 u   v
                                     underflow           no underflow

        rotation after:

                                                   i
                           +---------------------+---+--------------------+
            parent node    |                     | z |                    |
                           +---------------------+---+--------------------+
                                                 |   |
                             +-----------+---+---+   +-----------------+
            siblings         |           | x | y |   |                 |
                             +-----------+---+---+   +-----------------+
                                             |   |
                                             u   v


Right sibling is minimal:
    In that case we cannot chop off a key value pair and a child from the right
    sibling without violating the B tree invariant. The only possibility is to
    merge the two siblings.

    In order to merge the two sibling successfully, we have to push down the
    separator key value pair from the parent. This might cause an underflow in
    the parent depending on the size of the parent. A possible underflow in the
    parent must be repaired at the level of the parent of the parent.

    The parent looses one key value pair and one child with the merge. If the
    parent is the root node, the key value pair might be the last one leaving
    the parent with the merged child as the only child. In that case we can
    throw away the parent and use the merged child as the new root.

    The merge can never create an overflow. The merged node has ``2 * min_keys``
    key value pairs which is either the maximal number of keys or one less than
    the maximal number of keys.

    .. code-block:: none

        merge before:

                                                   i
                           +---------------------+---+--------------------+
            parent node    |                     | y |                    |
                           +---------------------+---+--------------------+
                                                 |   |
                                 +-----------+---+   +---+-----------------+
            siblings             |           | x |   | z |                 |
                                 +-----------+---+   +---+-----------------+
                                                 |   |
                                                 u   v
                                     underflow           minimal


        merge after:

                                                   i
                           +---------------------+--------------------+
            parent node    |                     |                    |
                           +---------------------+--------------------+
                                                 |
                                 +-----------+---+---+---+-----------------+
            siblings             |           | x | y | z |                 |
                                 +-----------+---+---+---+-----------------+
                                                 |   |
                                                 u   v



The following function handles the underflow condition properly in all cases. ::

    let handle_underflow
            (i: int)                (* Index of the child where the deletion occurred. *)
            (underflow_left: bool)  (* Underflow happend in the left child? *)
            (left_child: 'a t)
            (right_child: 'a t)
            (pair: Key.t * 'a)      (* The deleted key value pair. *)
            (pairs: 'a pairs)       (* The key value pairs of the parent. *)
            (children: 'a t array)  (* The children of the parent. *)
        : 'a delete
        =
        let not_minimal pairs1 pairs2 =
            if underflow_left then
                not_minimal pairs2
            else
                not_minimal pairs1
        in
        match left_child, right_child with
        | Leaf pairs1, Leaf pairs2 when not_minimal pairs1 pairs2 ->
            (* Right sibling is not minimal, rotate *)
            let pairs1, pairs, pairs2 =
                rotate_keys underflow_left i pairs1 pairs pairs2
            in
            let children =
                replace2 i (Leaf pairs1) (Leaf pairs2) children
            in
            {tree = Node (pairs, children); pair; underflow = false}

        | Leaf pairs1, Leaf pairs2 ->
            (* Sibling is minimal, merge *)
            merge_leaves i pair pairs1 pairs2 pairs children

        | Node (pairs1, children1), Node (pairs2, children2)
            when not_minimal pairs1 pairs2
            ->
            (* Sibling is not minimal, rotate *)
            let pairs1, pairs, pairs2 =
                rotate_keys underflow_left i pairs1 pairs pairs2
            and children1, children2 =
                rotate_children underflow_left children1 children2
            in
            let children =
                replace2
                    i
                    (Node (pairs1, children1))
                    (Node (pairs2, children2))
                    children
            in
            {tree = Node (pairs, children); pair; underflow = false}

        | Node (pairs1, children1), Node (pairs2, children2) ->
            (* Sibling is minimal, merge *)
            merge_nodes
                i pair
                pairs1 children1
                pairs2 children2
                pairs children

        | _, _ ->
            assert false (* Cannot happen, tree is balanced. *)



The function uses the following helper functions::

    let not_minimal (pairs: 'a pairs): bool =
        min_keys < Array.length pairs


    let replace2
            (i: int) (left: 'a t) (right: 'a t) (children: 'a t array)
        : 'a t array
        =
        let children = Array.copy children in
        assert (Array.valid_index i children);
        assert (Array.valid_index (i + 1) children);
        children.(i)     <- left;
        children.(i + 1) <- right;
        children


    let rotate_keys
            (to_left: bool)
            (i: int) (left: 'a pairs) (parent: 'a pairs) (right: 'a pairs)
        : 'a pairs * 'a pairs * 'a pairs
        =
        let open Array in
        assert (valid_index i parent);
        if to_left then
            push parent.(i) left,
            replace i (first right) parent,
            remove_first right
        else
            remove_last left,
            replace i (last left) parent,
            push_front parent.(i) right


    let rotate_children
            (to_left: bool)
            (left: 'a t array) (right: 'a t array)
        : 'a t array * 'a t array
        =
        let open Array in
        if to_left then
            push (first right) left,
            remove_first right
        else
            remove_last left,
            push_front (last left) right



    let merge_keys
            (i: int) (left: 'a pairs) (parent: 'a pairs) (right: 'a pairs)
        : 'a pairs * 'a pairs
        =
        assert (Array.valid_index i parent);
        let len_left  = Array.length left
        and len_right = Array.length right
        in
        let merged = Array.make (len_left + 1 + len_right) parent.(i)
        and parent = Array.remove i parent
        in
        Array.blit left  0 merged 0 len_left;
        Array.blit right 0 merged (len_left + 1) len_right;
        merged, parent


    let merge_leaves
            (i: int)
            (pair: Key.t * 'a)
            (pairs1: 'a pairs) (pairs2: 'a pairs)
            (pairs: 'a pairs) (children: 'a t array)
        : 'a delete
        =
        assert (i + 1 < Array.length children);
        let merged, pairs = merge_keys i pairs1 pairs pairs2
        and children      = Array.remove i children
        and underflow     = Array.length pairs <= min_keys
        in
        children.(i) <- Leaf merged;
        {tree = Node (pairs, children); pair; underflow}



    let merge_nodes
            (i: int)
            (pair: Key.t * 'a)
            (pairs1: 'a pairs) (children1: 'a t array)
            (pairs2: 'a pairs) (children2: 'a t array)
            (pairs: 'a pairs) (children: 'a t array)
        : 'a delete
        =
        assert (i + 1 < Array.length children);
        let pairs_new, pairs = merge_keys i pairs1 pairs pairs2
        and children      = Array.remove i children
        and underflow     = Array.length pairs <= min_keys
        and children_new  = Array.append children1 children2
        in
        children.(i) <- Node (pairs_new, children_new);
        {tree = Node (pairs, children); pair; underflow}
