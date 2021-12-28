************************************************************
Array
************************************************************



Binary Search
============================================================


Specification
----------------------------------------------------------------------

Binary search is an algorithm to find elements in an array or array like
structure which contains the elements in ascending order. In an array like
structure elements can be retrieved very fast by providing the index of the
element. The first index is ``0``, the second is ``1`` and the last is ``length
arr - 1``.

The array must not contain duplicates, otherwise the algorithm does not work as
expected. I.e. ::

    k0 < k1 < ... < km

must be satisfied where ``ki`` is the ``i``\ th key in the array and the array has
length ``m + 1``.

The binary search algorithm has the interface::

    let binsearch
        (compare: 'key -> 'key -> int)      (* Comparison function. *)
        (key_of:  'item -> 'key)            (* Extract a key from an item. *)
        (key:     'key)                     (* The key to be found. *)
        (arr:     'item array)              (* The array to be search in. *)
        : int * bool
        =
        ...


The search key is in many cases only a part of the items in the array. Therefore
we need a function ``key_of`` to extract the search key from an item. The
comparison function ``compare`` defines the order of the keys with the standard
semantics used in ocaml

.. code-block:: none

    compare a b < 0                 if and only if  a < b
    compare a b = 0                 if and only if  a = b
    compare a b > 0                 if and only if  a > b

It is required that the comparison function defines a linear order for the keys.

The function ``binsearch`` returns the pair ``(i, exact)``, where ``i`` is the
index of the key and the flag ``exact`` indicates that an exact match has been
found.

The function might return ``(len, false)`` where ``len`` is the length of
the array. Note that ``len`` is not a valid index into the array because the
index of the last element is ``len - 1``. For our reasoning we assume a
fictitious element ``arr.(len)`` which has the value ``+infinity``. Then the
return value ``i`` satisfies the specification::

    if exact then
        key = arr.(i)
    else
        key < arr.(i)

    arr.(j) < key       (* for all j < i *)

If an exact match has been found, then ``i`` is a valid index into the array. If
no exact match has been found, then ``i`` is the position where the search key
can be inserted without violating the order.



Basic Algorithm
----------------------------------------------------------------------

The basic search algorithm works between two valid indices ``lower`` and
``upper`` satisfying the invariant::

    0 <= lower < upper < length arr

    arr.(lower) < key < arr.(upper)


If ``lower + 1 = upper``, then no exact match can be found, because there is no
other element between the indices ``lower`` and ``upper``. Therefore the pair
``(upper, false)`` satisfies the specification.

If ``lower + 1 < upper``, then there is at least one index between ``lower`` and
``upper`` i.e. ``2 <= upper - lower`` and ``(upper - lower) / 2`` is at least
``1``.

The middle value ``mid = lower + (upper - lower) / 2`` lies strictly between the
indices ``lower`` and ``upper`` and is a valid index into the array. There are 3
possibilities:

- ``key < arr.(mid)``: We have to continue the search between ``lower`` and
  ``mid``.

- ``key = arr.(mid)``: We have found an exact match and return ``(mid, true)``.

- ``arr.(mid) < key``: We have to continue the search between ``mid`` and
  ``upper``.


We define the tail recursive helper function as follows::

    let rec search lower upper =
        (* Invariant:

            0 <= lower < upper < len

            arr.(lower) < key < arr.(upper)
        *)
        if lower + 1 = upper then
            upper, false
        else
            let mid = lower + (upper - lower) / 2 in
            let cmp = compare key (key_of arr.(mid)) in
                if cmp < 0 then
                    search lower mid
                else if cmp = 0 then
                    mid, true
                else
                    search mid upper





Complete Algorithm
----------------------------------------------------------------------

The basic algorithm requires an array length of at least ``2`` and that the
search key lies strictly between the first and the last element of the array. In
the complete algorithm we have to cover the corner cases there these conditions
are not satisfied i.e. the lengths zero and one and cases that the search key is
strictly less than or greater than all elements of the array.

.. code-block::

    let binsearch
        (compare: 'key -> 'key -> int)      (* Comparison function. *)
        (key_of:  'item -> 'key)            (* Extract a key from an item. *)
        (key:     'key)                     (* The key to be found. *)
        (arr:     'item array)              (* The array to be search in. *)
        : int * bool
        =
        let len = length arr
        in
        if len = 0 then
            len, false

        else if len = 1 then
            let cmp = compare key (key_of arr.(0)) in
            if cmp <= 0 then
                0, cmp = 0
            else
                len, false

        else
            (** length is at least 2! *)
            let rec search lower upper =
                ... (* see above *)
            in
            let lower, upper = 0, len - 1 in
            let cmp = compare key (key_of arr.(lower)) in
            if cmp <= 0 then
                (* key is less or equal the first element *)
                lower, cmp = 0
            else
                (* key is greater than the first element *)
                let cmp = compare key (key_of arr.(upper)) in
                if cmp < 0 then
                    (* invariant for [search] satisfied. *)
                    search lower upper
                else if cmp = 0 then
                    (* exact match with the last element *)
                    upper, true
                else
                    (* key is greater than all elements *)
                    len, false
