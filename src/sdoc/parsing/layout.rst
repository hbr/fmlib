********************************************************************************
Layout Parsing
********************************************************************************



The design of layout parsing in ``Fmlib`` is based on the work of
:cite:t:`adams2014layout2` with the following modifications:

- Tokens can by default appear to the right of the leftmost token of a
  construct. This modification makes the default handling more practical but
  does not change the semantics.

- The transition relation to describe the formal semantics has been extended to
  make parsing expressions not backtracking by default. A backtracking
  expression has to be annotated explicitly.



Parsing Expression Grammar
================================================================================

A parsing expression is either empty, a terminal, a nonterminal, a sequence of
two expressions or a biased choice i.e. an expression is defined by the grammar

.. math::

    e & ::= & \epsilon  & \text{ empty}
    \\
    & \mid & a          & \text{ terminal}
    \\
    & \mid & A          & \text{ nonterminal}
    \\
    & \mid & e e        & \text{ sequence}
    \\
    & \mid & e / e\quad & \text{ biased choice}
    \\
    & \mid & \overleftarrow e & \text{ backtrack}


A parsing expression grammar is a four tuple :math:`(T, N, S, \delta)` which
consists of a set of terminals (aka tokens), a set of nonterminals, the start
symbol which is a special nonterminal and a function mapping each nonterminal to
an expression.

The function :math:`\delta` can be extended to expressions such that

.. math::

    \delta(Ae) &:= \delta(A)e
    \\
    \delta(e_1\, /\, e_2) &:= \delta(e_1)\, / \, e_2
    \\
    \delta(ae) &:= ae
    \\
    \delta(\epsilon) &:= \epsilon

where :math:`A` is the nonterminal at the start of an expression. I.e. if an
expression does not start with a nonterminal, then the function :math:`\delta`
does not change the expression.

**Restriction**: Left recursion is forbidden. I.e. for all expressions :math:`e`
there has to be a natural number :math:`n` such that :math:`\delta^n(e)` either
is the empty expression or starts with a terminal (or is a biased choice where
the first alternative is either empty or starts with a terminal).

.. math::

    \delta^n(e) =
        \cases {
            \epsilon
            \\
            a e_1
            \\
            \epsilon\; / \ldots
            \\
            (a e_1)\; / \ldots
        }

In order to specify a certain layout we introduce the following additional
expressions.

.. math::

    e & ::= & &
    \\
    & \ldots
    \\
    & \mid & e^n\quad & e \text{ indented by $n$ columns}
    \\
    & \mid & |e| & \text{vertically aligned at first token}
    \\
    & \mid & e^\circledast & \text{detached}

where :math:`e^n` describes the same structure as :math:`e` but indented by
:math:`n` columns relative to its parent, :math:`|e|` describes the same
structure as :math:`e` but using its first token for vertical alignment and
:math:`e^\circledast` describes the structure :math:`e` detached from any
indentation and alignment requirements.





Formal Semantics
================================================================================


Indentation of a Sequence of Tokens
--------------------------------------------------------------------------------

An input stream :math:`u` consists of a finite sequence of tokens appearing at a
certain column. We notate the token :math:`a` appearing at column :math:`i` as
:math:`a_i`. Therefore we have

.. math::

    u = a_i\; b_j\; c_k\; \ldots\; z_l

as a valid input stream.

- The indentation of a token is the column at which the token appears in the
  input stream.

- The indentation of a nonempty subsequence of the input stream is
  the column of its leftmost token.

- In an aligned nonempty subsequence of the input stream the leftmost token must
  be the first token.



Transition Relation
--------------------------------------------------------------------------------

A parsing expression :math:`p` is executed within a state. The state consists of
the not yet consumed part of the input stream, an indentation set and an
alignment flag. The execution either succeeds (notated by :math:`\top w`) by
consuming a part of the input stream :math:`w` or fails after consuming a part
of the input stream (notated by :math:`\perp w`). In the success case we can
have a modified indentation set and a modified alignment flag. Therefore the
right hand side of the success case is annotated as :math:`\top^g_J w`.

.. math::

    (p, wu, I, f) & \Rightarrow &\top^g_J w
    \\
    (p, wu, I, f) & \Rightarrow &\perp w

In all transitions we maintain the invariant that the indentation set on the
right side of the transition is a subset of the indentation set on the left side
of the transition.

An indentation set is the set of allowed indentations for the parsing
expression.

Indentation sets are sets of natural numbers which can be described by pairs
:math:`[a,b]` where :math:`a` is the lower bound and :math:`b` is the upper
bound. The upper bound can be infinity :math:`\infty`. A valid indentation set
is never empty, i.e. we have :math:`a \le b`.

If the indentation set if :math:`[j, k]` where :math:`k \ne \infty` then
:math:`k` is the column of the (up to now) encountered leftmost token. If the
uppper bound is infinite, then no token has yet been encountered in the
construct.

The indentation set of the start expression is :math:`[0,\infty]`.


A parsing expression grammar successfully parses an input stream :math:`u`, if
and only if

.. math::

    (A, u, [0, \infty], -)\; \Rightarrow\; \top^f_{[j,k]} u

where :math:`A` is the start symbol of the grammar. The indentation of :math:`u`
is :math:`k`.



Empty Expression
--------------------------------------------------------------------------------

The empty parsing expression :math:`\epsilon` always succeeds by not consuming
any token an does not change the state.

.. math::

    (\epsilon, u, I, f) \Rightarrow \top^f_I \epsilon





Terminal
--------------------------------------------------------------------------------

Let us first consider the case that the aligment flag is not set. Then the
parsing expression :math:`a` succeeds if it encounters the token :math:`a_i` at
an allowed position :math:`i`. If :math:`[j,k]` is the indentation set then
:math:`j \le i` has to be satisfied for an allowed position. If the next token
on the input stream is not the expected token or it is offside, then the
expression fails.

.. math::

    \begin{array}{c}
        j \le i \land a = b
        \\
        \hline
        (a, b_i u, [j,k], -) \Rightarrow \top^{-}_{[j, \text{min } i\, k]} a
    \end{array}
    \quad
    \begin{array}{c}
        i < j \lor a \ne b
        \\
        \hline
        (a, b_i u, [j,k], -) \Rightarrow \perp \epsilon
    \end{array}


The input indentation set :math:`[j,k]` means that up to now the leftmost token
has been encountered at column :math:`k` (or no token has been encountered in
the surrounding construct and :math:`k = \infty`) and the minimal allowed column
is :math:`j`. The token :math:`a` might be the new leftmost token. Therefore on
success the upper bound of the indentation set might have to be updated.


Now we consider the case that the alignment flag is set. This means that we are
trying to align a construct and have not yet encountered its first token.
Because we are trying to align a construct within some indentation set and the
next token is the first and leftmost token of the construct, the next token must
be within this indentation set.

.. math::

    \begin{array}{c}
        i \in I \land a = b
        \\
        \hline
        (a, b_i u, I, +) \Rightarrow \top^{-}_{[i,i]} a
    \end{array}
    \quad
    \begin{array}{c}
        i \notin I \lor a \ne b
        \\
        \hline
        (a, b_i u, I, +) \Rightarrow \perp \epsilon
    \end{array}

In the success case the token is consumed, the indentation set consists only of
the column of the token and the aligment flag is reset. In case of failure
nothing is consumed.


Nonterminal
--------------------------------------------------------------------------------

If a parsing expression starts with a nonterminal, then the nonterminal has to
be mapped to its parsing expression by using the function :math:`\delta`.

.. math::

    \begin{array}{rcl}
        (\delta(A)e, u, I, f) & \Rightarrow & o
        \\
        \hline
        (Ae, u, I, f) & \Rightarrow & o
    \end{array}

Remember that left recursion is not allowed in a parsing expression grammar.
Therefore finally some terminal will appear as the first subexpression (or whole
expression becomes the empty expression).



Sequence
--------------------------------------------------------------------------------

For the sequence of two parsing expressions :math:`p_1 p_2` we have to
distinguish three cases:

- The first expression fails. This implies that the whole expression fails.

    .. math::

        \begin{array}{lcl}
            (p_1, w_1 w_2 u, I, f) & \Rightarrow & \perp w_1
            \\
            \hline
            (p_1 p_2, w_1 w_2 u, I, f) & \Rightarrow & \perp w_1
        \end{array}

- The first expression succeeds, but the second fails. This implies that the
  whole expression fails as well.

    .. math::

        \begin{array}{lcl}
            (p_1, w_1 w_2 u, I, f) & \Rightarrow & \top^g_J w_1
            \\
            (p_2, w_2 u, J, g) & \Rightarrow & \perp w_2
            \\
            \hline
            (p_1 p_2, w_1 w_2 u, I, f) & \Rightarrow & \perp (w_1 w_2)
        \end{array}


- Both expressions succeed. In that case the whole expression succeeds. The
  second expression uses the output state of the first as the input state. The
  final state of the whole expression is the final state of the second
  expression.

    .. math::

        \begin{array}{lcl}
            (p_1, w_1 w_2 u, I, f) & \Rightarrow & \top^g_J w_1
            \\
            (p_2, w_2 u, J, g) & \Rightarrow & \top^h_K w_2
            \\
            \hline
            (p_1 p_2, w_1 w_2 u, I, f) & \Rightarrow & \top^h_K (w_1 w_2)
        \end{array}




Biased Choice
--------------------------------------------------------------------------------

For the biased choice :math:`p_1\, /\, p_2` we have to distinguish some cases:

- The first expression succeeds. In that case the whole expression succeeds with
  the same output.

    .. math::

        \begin{array}{lcl}
            (p_1, w u, I, f) & \Rightarrow & \top^g_J w
            \\
            \hline
            (p_1\, /\, p_2, w u, I, f) & \Rightarrow & \top^g_J w
        \end{array}

- The first expression fails by consuming some tokens. In that case the whole
  expression fails with the same result.

    .. math::

        \begin{array}{lcl}
            (p_1, w u, I, f) & \Rightarrow & \perp w
            \\
            w \ne \epsilon
            \\
            \hline
            (p_1\, /\, p_2, w u, I, f) & \Rightarrow & \perp w
        \end{array}

- The first expression fails by not consuming any token. In that case the result
  of the whole expression is the result of the second expression.

    .. math::

        \begin{array}{lcl}
            (p_1, u, I, f) & \Rightarrow & \perp \epsilon
            \\
            (p_2, u, I, f) & \Rightarrow & o
            \\
            \hline
            (p_1\, /\, p_2, u, I, f) & \Rightarrow & o
        \end{array}



Backtrack
--------------------------------------------------------------------------------

The backtracking operator has no effect in the case of success. A failure with
consuming tokens is converted to a failure without consuming tokens.

    .. math::

        \begin{array}{lcl}
            (p, u w, I, f) & \Rightarrow & \top^g_J w
            \\
            \hline
            (\overleftarrow p, u, I, f) & \Rightarrow & \top^g_J w
        \end{array}
        \quad
        \begin{array}{lcl}
            (p, u w, I, f) & \Rightarrow & \perp u
            \\
            \hline
            (\overleftarrow p, u, I, f) & \Rightarrow & \perp \epsilon
        \end{array}



Alignment
--------------------------------------------------------------------------------

The expression :math:`|p|` describes an input sequence according to :math:`p`
where the first token is the leftmost token in the sequence and the sequence is
vertically aligned according to the first token.

Clearly alignment only makes sense if there are at least two vertically aligned
expressions. The expression :math:`|p|\, |q|` aligns the input sequences for
botch expressions vertically by using the first token of each sequence for the
alignment.

A sequence of aligned expressions have to be decoupled from the surrounding part
of the input stream by indentation. The expression

.. math::

    (|p|\, |q|\, \ldots)^n

aligns the input streams described by :math:`p`, :math:`q`, ... vertically and
indents the whole block by :math:`n` columns relative to the surrounding input
stream (note: the indentation can be zero). The decoupling by indentation
guarantees that the effect of the alignment is only local to the vertically
aligned blocks.


The transition of an aligned block is described by

.. math::

    \begin{array}{rcl}
        (p, wu, I, +) &\Rightarrow& o
        \\
        \hline
        (|p|, wu, I, f) &\Rightarrow& o\quad \text{adapt flag}
    \end{array}

It might be necessary to adapt the alignment flag in the output state to cover
the corner case :math:`p = \epsilon`. If the parsed sequence is not empty then
it has a last token. Since each token clears the alignment flag, the initial
alignment flag is cleared at the end. This is not the case for an empty
sequence.

Adaption: If the alignment flag is cleared at the end, no adaption is necessary.
If the alignment flag is not cleared at the end (only possible for an empty
sequence of tokens) then the alignment flag is set to its initial value
:math:`f`. This makes sure that an empty aligned sequence has no effect.



Indentation
--------------------------------------------------------------------------------


Indentation has no effect if the alignment flag is set.

.. math::

    \begin{array}{lcl}
        (p, wu, I, +) & \Rightarrow & o
        \\
        \hline
        (p^n, wu, I, +) & \Rightarrow & o
    \end{array}

If the alignment flag is not set, then the transition is described by the rules

.. math::

    \begin{array}{lcl}
        (p, wu, [i+n, \infty], -) & \Rightarrow & \top^f_{[k,l]} w
        \\
        \hline
        (p^n, wu, [i,j], -)
        & \Rightarrow
        & \top^f_{[i, \text{ min } j\, (l-n)]} w
    \end{array}
    \quad
    \begin{array}{lcl}
        (p, wu, I^n, -) & \Rightarrow & \perp w
        \\
        \hline
        (p^n, wu, I, -) & \Rightarrow & \perp w
    \end{array}

where :math:`i + n \le k \le l` and therefore :math:`i \le l - n` is guaranteed
because of the invariant.




Detachment
--------------------------------------------------------------------------------

If a parsing expression has some output in a completely unrestricted
environment, then the corresponding detached expression has the same output in
any environment except that the initial indentation and alignment state is
preserved. I.e. a detached expression runs independently from the indentation
and aligment requirements.


.. math::

    \begin{array}{lcl}
        (p, wu, [0, \infty], -) &\Rightarrow & \top^g_J w
        \\
        \hline
        (p^\circledast, wu, I, f) & \Rightarrow & \top^f_I w
    \end{array}
    \quad
    \begin{array}{lcl}
        (p, wu, [0, \infty], -) &\Rightarrow & \perp w
        \\
        \hline
        (p^\circledast, wu, I, f) & \Rightarrow & \perp w
    \end{array}




Implementation
================================================================================

In order to implement layout parsing with combinators we need an indentation set
and an aligment flag in the state.

.. code-block:: ocaml

    module Indent = struct
        type t = {
            lb:  int;           (* lower bound *)
            ub:  int option;    (* upper bound or infinity *)
            abs: bool;          (* aligment flag *)
        }

        let initial: t =
            {lb = 0; ub = None; align = false}
        ...
    end

For each token arriving at a certain column ``i`` we can check, if the token is
allowed at that column.

.. code-block:: ocaml

    let check_column (i: int) (ind: t): bool =
        ind.lb <= i
        &&
        (
            match ub with
            | Some ub when ind.abs ->
                i <= ub
            | _ ->
                true
        )

This function only checks the correct indentation. After this check it has to be
verified as usual if the token is the expected one.

If the token is in an allowed column and is an expected token, then the token
can be consumed.

.. code-block:: ocaml

    let consume (i: int) (ind: t): t =
        assert (check_column i ind);
        if not ind.abs then
            (* The token might be the new leftmost token. *)
            match ind.ub with
            | Some ub when ub <= i ->
                ind
            | _ ->
                {ind with ub = Some i}
        else
            (* First token in an aligned structure *)
            {
                lb  = i;
                ub  = Some i;
                abs = false;
            }

Remember: An upper bound, if present, marks the column of the leftmost token up
to now. The consumed token might be the new leftmost token. If this is the case,
the structure has to be updated.

The function ``align`` sets the alignment flag and the function ``end_align``
handles the corner case of an empty aligned structure.

.. code-block:: ocaml

    let align (ind: t): t =
        {ind with abs = true)

    let end_align (ind0: t) (ind: t): t =
        (* [ind0] is the indentation state at the start *)
        if not ind.abs then
            (* flag is cleared, the aligned sequence is not empty. *)
            ind
        else
            (* the aligned sequence is empty and therefore must not have any
               effect *)
            {ind with abs = ind0.abs}


In order to handle indentation properly we need functions to start and end an
indented block.

.. code-block:: ocaml

    let start_indent (i: int) (ind: t): t =
        assert (0 <= i);
        if ind.abs then
            (* No effect on aligned structures which have not yet received
               a first token. *)
            ind
        else
            match ind.ub with
            | None ->
                (* It does not make sense to indent relative to something
                   which does not yet have any token. *)
                ind
            | Some ub ->
                {
                    lb  = ub + i;
                    ub  = None;
                    abs = false;
                }

    let end_indent (ind0: t) (ind: t): t =
        if ind0.abs || ind0.ub = None then
            ind
        else
            ind0
