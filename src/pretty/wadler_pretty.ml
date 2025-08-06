open Printf



(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One Layout: Module Type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)

module type S0 =
sig
    type t

    val nil: t
    val text: string -> t
    val line: t
    val (<>): t -> t -> t
    val nest: int -> t -> t

    val layout: t -> string
end






(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One Layout
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Inefficient recursion for nesting and concatenation.

   Concatenation

        (x <> y) <> z

        First x is traversed completely to append y to all line and text
        elemenst. And then x <> y is traversed completely to append z.

   Nesting

        nest i (nest j x)

        nest j x adds the indentation j to all lines in x and then nest i adds i
        again. This is quadratic complexity on the length of the document.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)
module Doc0: S0 =
struct
    type t =
        | Nil
        | Text of string * t
        | Line of int * t

    let nil    = Nil

    let text s = Text (s, Nil)

    let line   = Line (0, Nil)


    let rec (<>) x y =
        match x with
        | Nil ->
            y
        | Text (s, Nil) -> Text (s, y)
        | Text (s, x)   -> Text (s, x <> y)
        | Line (i, Nil) -> Line (i, y)
        | Line (i, x)   -> Line (i, x <> y)


    let rec nest i = function
        | Nil -> Nil
        | Text (s, x) -> Text (s, nest i x)
        | Line (j, x) -> Line (i + j, nest i x)


    let rec layout = function
        | Nil         ->  ""
        | Text (s, x) -> s ^ layout x
        | Line (i, x) -> sprintf "\n%s%s" (String.make i ' ') (layout x)
end








(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One Layout Lazy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   No gain in efficiency if layout is called. All lazy terms have to be forced.
   The inefficiency for nesting and concatenation remains.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)
module Doc0_lazy:
sig
    type t

    val nil: t
    val text: string -> t
    val line: t
    val (<>): t -> t -> t
    val nest: int -> t -> t

    val layout: t -> string
end
=
struct
    type t =
        | Nil
        | Text of string * laz
        | Line of int * laz

    and laz = t Lazy.t

    let force = Lazy.force


    let nil    = Nil

    let text s = Text (s, lazy Nil)

    let line   = Line (0, lazy Nil)


    let rec (<>) x y =
        match x with
        | Nil -> y
        | Text (s, x) -> begin
                match force x with
                | Nil -> Text (s, lazy y)
                | x   -> Text (s, lazy (x <> y))
            end
        | Line (i, x) -> begin
                match force x with
                | Nil -> Line (i, lazy y)
                | x   -> Line (i, lazy (x <> y))
            end


    let rec nest i = function
        | Nil -> Nil
        | Text (s, x) -> Text (s,     lazy (nest i (force x)))
        | Line (j, x) -> Line (i + j, lazy (nest i (force x)))


    let rec layout = function (* document must not contain unions *)
        | Nil         ->  ""
        | Text (s, x) -> s ^ (force x |> layout)
        | Line (i, x) -> sprintf "\n%s%s" (String.make i ' ') (force x |> layout)
end






(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One Layout Optimized
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Key ideas to avoid the quadratic complexity for left associative
    concatenation and nested nesting:

    - Represent concatenation x <> y by an own constructor Cat (x, y) and layout
    x completely before laying out y.

    - Represent nest i x by an own constructor Nest (i, x) and cumulate
   the nesting levels.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)
module Doc0_opt: S0 =
struct
    type t =
        | Nil
        | Text of string
        | Line
        | Cat  of t * t
        | Nest of int * t

    let nil    = Nil

    let text s = Text s

    let line   = Line

    let (<>) x y = Cat (x, y)

    let nest i x = Nest (i, x)


    let rec la: (int * t) list -> string = function
        | [] ->
            ""
        | (i, x) :: z ->
            match x with
            | Nil         -> ""
            | Text s      -> s
            | Line        -> sprintf "\n%s" (String.make i ' ')
            | Nest (j, x) -> la (((i + j), x) :: z)         (* cumulate *)
            | Cat (x, y)  -> la ((i, x) :: (i, y) :: z)     (* x first  *)


    let layout (x: t): string = la [0, x]
end










(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Module Type for Multiple Layouts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   One additional function group.

   Function pretty with a desired line width is exported instead of layout.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)
module type S =
sig
    type t

    val nil: t
    val text: string -> t
    val line: t
    val (<>): t -> t -> t
    val nest: int -> t -> t
    val group: t -> t

    val pretty: int -> t -> string
end












(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Multiple Layouts (Inefficient)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Each group doubles the documents i.e. for 10 groups we have 2^10 documents.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)
module Doc1: S =
struct
    type t =
        | Nil
        | Text of string * t
        | Line of int * t
        | Union of t * t

    let rec flatten: t -> t = function
        | Nil          -> Nil
        | Text (s, x)  -> Text (s,   flatten x)
        | Line (_, x)  -> Text (" ", flatten x)
        | Union (x, _) -> flatten x


    let rec fits (w: int): t -> bool = function
        (* Document must not contain unions *)
        | Nil          -> true
        | Line (_, _)  -> true
        | Union _      -> assert false (* illegal call *)
        | Text (s, x)  ->
            assert (0 <= w);
            let w = w - String.length s in
            w < 0 || fits w x


    let rec best (w: int) (k: int): t -> t = function
        | Nil -> Nil
        | Text (s, x) -> Text (s, best w (k + String.length s) x)
        | Line (i, x) -> Line (i, best w i x)
        | Union (x, y) ->
            if fits (w - k) (best w k x) then
                x
            else
                best w k y


    let rec layout = function (* document must not contain unions *)
        | Nil         ->  ""
        | Text (s, x) -> s ^ layout x
        | Line (i, x) -> sprintf "\n%s%s" (String.make i ' ') (layout x)
        | Union _     -> assert false (* illegal call *)



    let nil = Nil

    let text s = Text (s, Nil)

    let line   = Line (0, Nil)

    let rec (<>) x y =
        match x with
        | Nil ->
            y
        | Text (s, Nil) -> Text (s, y)
        | Text (s, x)   -> Text (s, x <> y)
        | Line (i, Nil) -> Line (i, y)
        | Line (i, x)   -> Line (i, x <> y)
        | Union (x1, x2)  -> Union (x1 <> y, x2 <> y)

    let rec nest i = function
        | Nil -> Nil
        | Text (s, x) -> Text (s, nest i x)
        | Line (j, x) -> Line (i + j, nest i x)
        | Union (x, y) -> Union (nest i x, nest i y)


    let group (x: t): t =
        Union (flatten x, x)

    let pretty (w: int) (x: t): string =
        layout (best w 0 x)
end














(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Multiple Layouts Lazy Implementation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Basic idea:

        The constructors Text (s, x), Line (i, x), and Union (x, y) do not
        contain a document x, but a lazy document.

        The function fits forces the document x in Text (s, x) only as long as
        the document fits on the line.

        The function best is lazy as well and forces documents only through
        fits. If fits returns true, then the document is forced completely. This
        is not a problem, since it is selected and used in the layout.

        If fits fails the document is only partially forced until the failure is
        evident and then thrown away, because the unflatted version is the
        selected one.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)
module Doc1_lazy: S =
struct
    type t =
        | Nil
        | Text of string * laz
        | Line of int * laz
        | Union of laz * t

    and laz = t Lazy.t

    let force = Lazy.force


    let rec flatten: t -> laz = function
        | Nil          -> lazy Nil
        | Text (s, x)  -> lazy (Text (s,   force x |> flatten))
        | Line (_, x)  -> lazy (Text (" ", force x |> flatten))
        | Union (x, _) -> force x |> flatten


    let rec fits (w: int): t -> bool = function
        (* Document must not contain unions *)
        | _ when w < 0 -> false
        | Nil | Line _ -> true
        | Union _      -> assert false (* illegal call *)
        | Text (s, x)  ->
            fits (w - String.length s) (force x)


    let rec best (w: int) (k: int): t -> t = function
        | Nil ->
            Nil

        | Text (s, x) ->
            Text (s, lazy (best w (k + String.length s) (force x)))

        | Line (i, x) ->
            Line (i, lazy (best w i (force x)))

        | Union (x, y) ->
            let x = force x |> best w k in
            if fits (w - k) x then
                x
            else
                best w k y


    let rec layout = function (* document must not contain unions *)
        | Nil         ->  ""
        | Text (s, x) -> s ^ layout (force x)
        | Line (i, x) -> sprintf "\n%s%s" (String.make i ' ') (layout (force x))
        | Union _     -> assert false (* illegal call *)



    let nil = Nil

    let text s = Text (s, lazy Nil)

    let line   = Line (0, lazy Nil)

    let rec (<>) x y =
        match x with
        | Nil ->
            y
        | Text (s, x) -> begin
                match force x with
                | Nil -> Text (s, lazy y)
                | x   -> Text (s, lazy (x <> y))
            end
        | Line (i, x) -> begin
                match force x with
                | Nil -> Line (i, lazy y)
                | x   -> Line (i, lazy (x <> y))
            end
        | Union (x1, x2)  ->
            Union (lazy (force x1 <> y), x2 <> y)

    let rec nest i = function
        | Nil -> Nil
        | Text (s, x)  -> Text (s,     lazy (nest i (force x)))
        | Line (j, x)  -> Line (i + j, lazy (nest i (force x)))
        | Union (x, y) -> Union (lazy (nest i (force x)), nest i y)


    let group (x: t): t =
        Union (flatten x, x)

    let pretty (w: int) (x: t): string =
        layout (best w 0 x)
end













(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Multiple Layouts Optimized Lazy Implementation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)
module Doc2_lazy: S =
struct
    let force = Lazy.force

    module Base =
    struct
        type t =
            | Nil
            | Text of string * laz
            | Line of int * laz

        and laz = t Lazy.t

        let rec fits (w: int): t -> bool = function
            | _ when w < 0      -> false
            | Nil | Line (_, _) -> true

            | Text (s, x) ->
                fits (w - String.length s) (force x)


        let rec layout: t -> string = function
            | Nil ->
                ""
            | Text (s, x) ->
                sprintf "%s%s" s (force x |> layout)

            | Line (i, x) ->
                sprintf "\n%s%s" (String.make i ' ') (force x |> layout)
    end

    type t =
        | Nil
        | Text of string
        | Line
        | Cat of laz * laz
        | Nest of int * laz
        | Union of laz * t

    and laz = t Lazy.t


    let rec flatten: t -> laz = function
        | Nil         ->  lazy Nil
        | Line        ->  lazy (Text " ")
        | Text _ as x ->  lazy x
        | Cat (x, y)  ->  lazy (Cat (flatlaz x, flatlaz y))
        | Nest (i, x) ->  lazy (Nest (i, flatlaz x))
        | Union (x, _) -> flatlaz x

    and flatlaz x = force x |> flatten


    let rec be (w: int) (k: int): (int * t) list -> Base.t = function
        | [] ->
            Nil

        | (i, x) :: z ->
            match x with
            | Nil ->
                be w k z

            | Text s ->
                Text (s, lazy (be w (k + String.length s) z))

            | Line ->
                Line (i, lazy (be w i z))

            | Cat (x, y) ->
                (* Make concatenation right associative *)
                be w k ((i, force x) :: (i, force y) :: z)

            | Nest (j, x) ->
                (* Accumulate nesting levels *)
                be w k (((i + j), force x) :: z)

            | Union (x, y) ->
                let x = be w k ((i, force x) :: z) in
                if Base.fits (w - k) x then
                    x
                else
                    be w k ((i, y) :: z)



    let nil = Nil

    let text s = Text s

    let line   = Line

    let (<>) x y = Cat (lazy x, lazy y)

    let nest i x = Nest (i, lazy x)

    let group = function
        | Union _ as x -> x (* group is idempotent *)
        | x            -> Union (flatten x, x)

    let pretty (w: int) (x: t): string =
        be w 0 [0, x] |> Base.layout
end











module Pretty_plus (Pretty: S):
sig
    include S

    val parent_child: int -> t -> t -> t
end
=
struct
    include Pretty

    let parent_child (i: int) (parent: t) (child: t): t =
        parent <> nest i (line <> group child) |> group
end



module Tree (Pretty: S) =
struct
    module P = Pretty_plus (Pretty)

    type t =
        { name: string; children: t list; }


    let leaf (name: string): t =
        {name; children = []}

    let tree (name: string) (children: t list): t =
        {name; children}



    let doc_tree (t: t): P.t =
        let open P
        in
        let rec doc is_top tree =
            match tree.children with
            | [] ->
                text tree.name
            | _ ->
                let d =
                    parent_child
                        2
                        (text tree.name)
                        (children tree.children)
                in
                if is_top then
                    d
                else
                    text "(" <> d <> text ")"
        and children lst =
            match lst with
            | [last] ->
                doc false last
            | head :: tail ->
                doc false head
                <> line
                <> children tail
            | [] ->
                assert false (* [lst] is never empty *)
        in
        doc true t

    let args2 = [leaf "a"; leaf "b"]
    let args3 = [leaf "a"; leaf "b"; leaf "c"]
    let args4 = [leaf "a"; leaf "b"; leaf "c"; leaf "d"]

    let tree0 =
        tree "ff" args3

    let tree1 =
        tree
            "ff"
            [leaf "a";
             tree "gf" args2;
             leaf "d"]

    let tree2 =
        tree
            "ff"
            [tree "gf" args2;
             tree "gf" args2;
             tree "gf" args2]


    let tree3 =
        tree
            "f"
            [tree "f" args2;
             tree "f" [tree "f" args4];
             tree "f" args4;
             tree "f" args3;]


    let string0 w =
        doc_tree tree0 |> P.pretty w

    let string1 w =
        doc_tree tree1 |> P.pretty w

    let string2 w =
        doc_tree tree2 |> P.pretty w

    let string3 w =
        doc_tree tree3 |> P.pretty w
end



let test0 (print: bool) (str: string) (expected: string): bool =
    let ok = str = expected in
    if not ok || print then
        printf "---\nstr\n%s\nexpected\n%s\n\n" str expected;
    ok


module Test (P: S) =
struct
    open Unit_test_support

    module Tree = Tree (P)

    let%test _ =
        let str = Tree.string0 3
        and expected = {|
            ff
              a
              b
              c
            |} |> quoted
        in
        test0 false str expected

    let%test _ =
        let str = Tree.string0 8
        and expected = "ff a b c"
        in
        test0 false str expected

    let%test _ =
        let str = Tree.string0 7
        and expected = {|
            ff
              a b c
            |} |> quoted
        in
        test0 false str expected


    let%test _ =
        let str = Tree.string2 27
        and expected = {|
            ff
              (gf a b)
              (gf a b)
              (gf a b)
            |} |> quoted
        in
        test0 false str expected


    let%test _ =
        let str = Tree.string3 47
                (*      01234567890123456789012345678901234567890123456 *)
        and expected = "f (f a b) (f (f a b c d)) (f a b c d) (f a b c)"
        in
        test0 false str expected


    let%test _ =
        let str = Tree.string3 46
        and expected = {|
            f
              (f a b)
              (f (f a b c d))
              (f a b c d)
              (f a b c)
            |} |> quoted
        in
        test0 false str expected


    let%test _ =
        let str = Tree.string3 16
        and expected =
            {|
            f
              (f a b)
              (f
                (f a b c d))
              (f a b c d)
              (f a b c)
            |} |> quoted
        in
        test0 false str expected


    let%test _ =
        let str = Tree.string3 13
        and expected =
            {|
            f
              (f a b)
              (f
                (f
                  a
                  b
                  c
                  d))
              (f a b c d)
              (f a b c)
            |} |> quoted
        in
        test0 false str expected


    let%test _ =
        let str = Tree.string3 5
        and expected =
            {|
            f
              (f
                a
                b)
              (f
                (f
                  a
                  b
                  c
                  d))
              (f
                a
                b
                c
                d)
              (f
                a
                b
                c)
            |} |> quoted
        in
        test0 false str expected
end

module Test_Doc1_lazy = Test (Doc1_lazy)
module Test_Doc2_lazy = Test (Doc2_lazy)
