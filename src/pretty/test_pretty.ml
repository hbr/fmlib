(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unit Tests with Trees
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)
module Tree =
struct
    type t =
        { name: string; children: t list; }


    let leaf (name: string): t =
        {name; children = []}

    let tree (name: string) (children: t list): t =
        {name; children}



    let doc_tree (t: t): Pretty.t =
        let open Pretty
        in
        let rec doc is_top tree =
            match tree.children with
            | [] ->
                text tree.name
            | _ ->
                let d =
                    parent_child
                        " "
                        2
                        (text tree.name)
                        (children tree.children)
                in
                if is_top then
                    d
                else
                    text "(" <+> d <+> text ")"
        and children lst =
            match lst with
            | [last] ->
                doc false last
            | head :: tail ->
                doc false head
                <+> space
                <+> children tail
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

    let string w r tree =
        Pretty.(doc_tree tree |> layout_with_ribbon w r |> to_string)

    let string0 w = string w w tree0
    let string1 w = string w w tree1
    let string2 w = string w w tree2
    let string3 w = string w w tree3

    let string3r w r = string w r tree3

    let _ = string1


    open Unit_test_support


    let%test _ =
        let str = string0 3
        and expected = {|
            ff
              a
              b
              c
            |} |> quoted
        in
        check false str expected

    let%test _ =
        let str = string0 8
        and expected = "ff a b c"
        in
        check false str expected

    let%test _ =
        let str = string0 7
        and expected = {|
            ff
              a b c
            |} |> quoted
        in
        check false str expected


    let%test _ =
        let str = string2 27
        and expected = {|
            ff
              (gf a b)
              (gf a b)
              (gf a b)
            |} |> quoted
        in
        check false str expected


    let%test _ =
        let str = string3 47
        (*      01234567890123456789012345678901234567890123456 *)
        and expected = "f (f a b) (f (f a b c d)) (f a b c d) (f a b c)"
        in
        check false str expected


    let%test _ =
        let str = string3 46
        and expected = {|
            f
              (f a b)
              (f (f a b c d))
              (f a b c d)
              (f a b c)
            |} |> quoted
        in
        check false str expected


    let%test _ =
        let str = string3 16
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
        check false str expected


    let%test _ =
        let str = string3 13
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
        check false str expected


    let%test _ =
        let str = string3r 47 12
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
        check false str expected


    let%test _ =
        let str = string3 5
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
        check false str expected
end











(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unit Tests Word Wrap
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)

module Wrap =
struct
    open Unit_test_support

    let s1 = {|
        ab ab
        a
        b
        c
        ab |}

    let wrap w s =
        Pretty.(wrap_words s |> layout w |> to_string)

    let wrap_list w xs =
        Pretty.(wrap_words_list xs |> layout w |> to_string)

    let ws1 w = wrap w s1



    let%test _ =
        check
            false
            (ws1 5)
            ({|
                ab ab
                a b c
                ab|} |> quoted)

    let%test _ =
        check
            false
            (wrap_list 5 [s1; s1])
            ({|
                ab ab
                a b c
                ab ab
                ab a
                b c
                ab|} |> quoted)

    let%test _ =
        check
            false
            (wrap 5 "aa b c d")
            ({|
                aa b
                c d|} |> quoted)
end
