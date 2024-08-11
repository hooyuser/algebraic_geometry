#import "@preview/cetz:0.2.2"
#import "@preview/fletcher:0.5.1" as fletcher: diagram, node, edge
#import "@preview/xarrow:0.3.1": xarrow


#import "@local/math-notes:0.1.0": *

#show: math_notes

// Title Page
#v(1fr)
#align(center)[
  #text(font: "Noto Serif", size: 35pt, weight: 500)[#smallcaps[ALGEBRAIC GEOMETRY]]
  #v(1.5fr)
  #text(font: "Noto Serif", size: 15pt, datetime.today().display())
]
#v(1.2fr)

#pagebreak()

#block(inset: (left: -0.5em, right: -0.5em))[
  #outline(title: text(font: "Noto Sans", size: 23pt, weight: 700, stretch: 150%)[Contents #v(1em)], depth: 3)
]

#pagebreak()

#let cal(x) = math.class("unary", text(font: "Computer Modern Symbol", x))


#let tildecal(x) = if (x.text == "F") {
  $accent(cal(F)#h(0.3em), ~)#h(-0.3em)$
} else {
  $tilde(cal(#x))$
}


#let sheafify(x) = $cal(#x)^(#h(0.2em)op("sh"))$

#let mathsf(x) = $sans(upright(#x))$

#let res(V, U) = $op("res")_(#V arrow.l.hook #U)$

#let affine = $bold(upright(A))$

#let injlim = $limits(limits(lim)_(xarrow(#v(-50em), width: #1.8em)))$
#let projlim = $limits(limits(lim)_(xarrow(sym:arrow.l.long, #v(-50em), width: #1.8em)))$


#counter(page).update(1)
= Sheaf Theory <sheaf-theory>
== Presheaf <presheaf>
=== Presheaf on Topological Space <presheaf-on-topological-space>

#definition[
  Category of Open Sets
][
  Let $lr((X , tau))$ be a topological space. Then $lr((tau , subset.eq))$ is a filtered set which can be seen as a
  filtered (0,1)-category. This category is called the #strong[category of open sets of $X$], denoted as $mathsf("Open")_X$.
  The explicit description of $mathsf("Open")_X$ is as follows:

  - Objects: Open sets of $X$.

  - Morphisms: Inclusion maps of open sets.

  Note that $lr((tau , supset.eq))$ is also a filtered set which can be seen as the filtered (0,1)-category $mathsf("Open")_X^(upright(o p))$.
  Therefore, $mathsf("Open")_X$ is both filtered and cofiltered.
]

Note in $mathsf("Open")_X$, all diagrams are commutative.



#definition[
  $mathsf(C)$-valued Presheaf
][
  Let $mathsf(C)$, $mathsf(D)$ be categories. A $mathsf(C)$-valued #strong[presheaf] is a functor $F : mathsf(D)^(upright(o p)) arrow.r mathsf(C)$.
]


#definition[
  $mathsf(C)$-valued Presheaf on a Topological Space
][
  Let $X$ be a topological space and $mathsf(C)$ be a category. The #strong[category of $mathsf(C)$-valued presheaves on $X$] is
  defined as
  $ mathsf("PSh")_(mathsf(C)) lr((X)) := lr([mathsf("Open")_X^(op("op")) , mathsf(C)]) , $
  which is the category of contravariant functors from $mathsf("Open")_X$ to $mathsf(C)$.

]
If $mathsf(C)$ is an abelian category, then $mathsf("PSh")_(mathsf(C)) lr((X))$ is an abelian category.



#definition[
  $mathsf("Set")$-valued Presheaf on a Topological Space
][
  Let $X$ be a topological space. The #strong[category of $mathsf("Set")$-valued presheaves on $X$] (or #strong[category of presheaves of sets on $X$])
  is defined as
  $ mathsf("PSh")_(mathsf("Set")) lr((X)) := lr([mathsf("Open")_X^(op("op")) , mathsf("Set")]) . $
  The objects and morphisms of $mathsf("PSh")_(mathsf("Set")) lr((X))$ can be described explicitly as follows:

  - #block[Objects: A #strong[presheaf of sets on $X$] is a contravariant functor $cal(F)$ that can be depicted as follows:
      #functor_diagram(
        F: $cal(F)$,
        C: $mathsf("Open")_X^(op("op"))$,
        D: $mathsf("Set")$,
        g: $iota^(op("op"))$,
        X: $V$,
        Y: $U$,
        Fg: $op("res")_(V,U)$,
        FX: $cal(F)(V)$,
        FY: $cal(F)(U)$,
      )
      Therefore, for any open sets $U , V$ of $X$ such that $U subset.eq V$, there is a map $op("res")_(V , U) : cal(F) lr((V)) arrow.r cal(F) lr((U))$,
      called the #strong[restriction map], satisfying the following conditions:

      + $res(U , U) = op("id")_(cal(F) lr((U)))$.

      + $res(W , U) = res(V, U) circle.stroked.tiny res(W , V)$ whenever $U subset.eq V subset.eq W$.
    ]

  - Morphisms: A #strong[morphism of presheaves of sets on $X$] is a natural transformation $phi : cal(F) arrow.r cal(G)$.
    The naturality of $phi$ means that for any open sets $U , V$ of $X$ such that $U subset.eq V$, the following diagram
    commutes
    #square_cd(
      A11: $cal(F)(V)$,
      A12: $cal(F)(U)$,
      A21: $cal(G)(V)$,
      A22: $cal(G)(U)$,
      Ff: $op("res")_(V arrow.l.hook U)$,
      Gf: $op("res")_(V arrow.l.hook U)$,
      theta_l: $phi_(V)$,
      theta_r: $phi_(U)$,
    )
]
#definition[Sections of a Presheaf][
  Let $cal(F)$ be a presheaf of sets on a topological space $X$. A #strong[section] of $cal(F)$ over an open set $U subset.eq X$ is
  defined as an element in $cal(F) lr((U))$.
]

$Gamma lr((U , cal(F)))$ is another notation often used to indicate sections, which means we have the tautological
equality $Gamma lr((U , cal(F))) = cal(F) lr((U))$.

#example[Constant Presheaf][
  Let $X$ be a topological space and $mathsf(C)$ be a category. Suppose $A in upright(O b) lr((mathsf(C)))$. The #strong[constant presheaf on $X$ with value $A$] is
  defined as follows:

  #functor_diagram(
    F: $cal(F)$,
    C: $mathsf("Open")_X^(op("op"))$,
    D: $mathsf("Set")$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $U$,
    Fg: $op("id")_A$,
    FX: $A$,
    FY: $A$,
  )
]<constant-presheaf>

=== Stalk of a Presheaf <stalk-of-a-presheaf>
#definition[
  Stalk of a $mathsf(C)$-valued Presheaf
][
  Let $cal(F)$ be a $mathsf(C)$-valued presheaf on a topological space $lr((X , tau))$. The #strong[stalk] of $cal(F)$ at
  a point $x in X$ is defined as the colimit
  $
    cal(F)_x := injlim_(x in U in tau) cal(F) lr((U)) .
  $

  where $U$ runs over all open neighborhoods of $x$. Formally, let $mathsf(O p e n)_(X , x)$ be the full subcategory of $mathsf(O p e n)_X$ whose
  objects are the open neighborhoods of $x$ and whose morphisms are the inclusions of open sets. Since for any $U , V in tau$,
  there exists $U sect V in tau$ such that $x in U sect V$, $U supset.eq U sect V$ and $V supset.eq U sect V$, we see $mathsf(O p e n)_(X , x)^(upright(o p))$ is
  a filtered category. Therefore, $cal(F)_x$ is a filtered colimit
  $
    cal(F)_x = injlim cal(F) |_mathsf("Open")_(X , x)^(op("op"))
  $

]
#definition[
  Stalk of a $mathsf(S e t)$-valued Presheaf
][
  Let $lr((X , tau))$ be a topological space. For any $mathsf(S e t)$-valued presheaf $cal(F)$ on $X$ and for any $x in X$,
  the #strong[stalk] of $cal(F)$ at a point $x in X$ always exists because in $mathsf(S e t)$ all filtered colimits
  exists. The stalk $cal(F)_x$ can described explicitly as the quotient set

  $
    cal(F)_x = injlim cal(F) |_mathsf("Open")_(X , x)^(op("op"))=(
      product.co_(U in tau , U in.rev x) cal(F) (U)
    ) \/ tilde.op = {(U , f) divides x in U in tau , f in cal(F) (U)} \/ tilde.op
  $

  where $tilde.op$ is the equivalence relation defined as follows: for any open neighborhoods $U , V$ of $x$ and any $f in cal(F) lr((U))$, $g in cal(F) lr((V))$,
  $
    lr((U , f)) tilde.op lr((V , g)) arrow.l.r.double upright("there exists an open neighborhood ") W subset.eq U sect V upright("of ") x upright("such that ") f lr(|""_W = g|)_W .
  $
  The image under the map $cal(F) lr((U)) arrow.r.hook cal(F)_x$ of a section $f in cal(F) lr((U))$ is the equivalence
  class of $lr((U , f))$, denoted as $lr([lr((U , f))])_x$, called the #strong[germ] of $f$ at $x$.

]
#proposition[][
  Let $mathsf(C)$ be a category. Let $F : mathsf(C) arrow.r mathsf(S e t)$ be a functor. Assume that

  #block[
    #set enum(numbering: "(i)", start: 1)
    + $F$ is faithful,

    + directed colimits exist in $mathsf(C)$ and $F$ commutes with them.
  ]

  Let $cal(F)$ be a $mathsf(C)$-valued presheaf on a topological space $X$ and $x in X$. Then

  $
    cal(F)_x := injlim_(x in U in tau) cal(F) lr((U)) .
  $
  exists in $mathsf(C)$. Its underlying set is equal to the stalk of the underlying presheaf of sets of $cal(F)$, i.e. $F lr((cal(F)_x)) = lr((F circle.stroked.tiny cal(F)))_x$.

]



#definition[
  Stalk Functor
][
  The construction of $cal(F)_x$ is functorial in the presheaf $cal(F)$. In other words, we can define a #strong[stalk functor] $injlim_(x in U in tau) cal(F) lr((U)) :mathsf("PSh")_(mathsf(C))(X)->mathsf(C)$ as
  follows

  #align(
    center,
    grid(
      columns: (auto, auto),
      rows: (auto,),
      align: center + horizon,
      inset: (left: 1em, right: 1em, top: 0.5em, bottom: 0.6em),
      grid.cell(
        functor_diagram(
          F: $injlim_(x in U in tau)$,
          C: $mathsf("PSh")_(mathsf(C))(X)$,
          D: $mathsf(C)$,
          g: $phi$,
          X: $cal(F)$,
          Y: $cal(G)$,
          Fg: $$,
          FX: $cal(F)_x$,
          FY: $cal(G)_x$,
        ),
        inset: (right: 4em),
      ),
      grid.vline(),
      grid.cell(
        functor_diagram(
          F: $injlim_(x in U in tau)$,
          C: $mathsf("PSh")_(mathsf(C))(X)$,
          D: $mathsf("Set")$,
          g: $phi$,
          X: $cal(F)$,
          Y: $cal(G)$,
          Fg: $$,
          FX: $cal(F)_x$,
          FY: $cal(G)_x$,
          Fg_e: $$,
          FX_e: $(U,f)$,
          FY_e: $lr((U, phi_U (f)))$,
        ),
        inset: (left: 4em),
      ),
    ),
  )

]


=== Presheaf on Topological Base
<presheaf-on-a-base-for-topology-space>
#definition[
  Category of $mathsf(C)$-valued Presheaves on a Base for Topology
][
  Let $X$ be a topological space. Let $cal(B)$ be a base for the topology on $X$. The #strong[category of $mathsf(C)$-valued presheaves on $cal(B)$] is
  defined as $ mathsf("PSh")_(mathsf(C)) lr((cal(B))) := lr([mathsf(B)^(upright(o p)) , mathsf(C)]) , $ where $mathsf(B)$ is the
  category whose objects are the elements of $cal(B)$ and whose morphisms are the inclusions of elements of $cal(B)$.

]
#definition[
  Stalk of a $mathsf(C)$-valued Presheaf on a Base for Topology
][
  Let $cal(F)$ be a $mathsf(C)$-valued presheaf on a topological space $X$. Let $cal(B)$ be a base for the topology on $X$.
  For any $x in X$, the #strong[stalk] of $cal(F)$ at a point $x in X$ is defined as the colimit
  $
    cal(F)_x := injlim_(x in B in cal(B)) cal(F) lr((B)) .
  $
  where $B$ runs over all elements of $cal(B)$ containing $x$. Formally, let $mathsf(B)_x$ be the full subcategory of $mathsf(B)$ whose
  objects are the elements of $cal(B)$ containing $x$ and whose morphisms are the inclusions of elements of $cal(B)$.
  Since for any $B , C in cal(B)$ such that $x in B sect C$, there exists $D in cal(B)$ such that $x in D subset.eq B sect C$,
  we see $mathsf(B)_x^(upright(o p))$ is a filtered category. Therefore, $cal(F)_x$ is a filtered colimit
  $
    cal(F)_x = injlim cal(F) |_mathsf(B)_(x)^(op("op"))
  $
]


#definition[
  Stalk of a $mathsf("Set")$-valued Presheaf on a Base for Topology
][
  Let $X$ be a topological space. Let $cal(B)$ be a base for the topology on $X$. For any $mathsf("Set")$-valued presheaf $cal(F)$ on $cal(B)$ and
  for any $x in X$, the #strong[stalk] of $cal(F)$ at a point $x in X$ always exists because in $mathsf("Set")$ all
  filtered colimits exists. The stalk $cal(F)_x$ can described explicitly as the quotient set
  $
    cal(F)_x = injlim cal(F) |_mathsf(B)_(x)^(op("op"))= (
      product.co_(B in cal(B) , x in B) cal(F) (B)
    ) \/ op(tilde.op) = {(B , f) divides x in B in cal(B) , f in cal(F) (B)} \/ tilde.op
  $
  where $tilde.op$ is the equivalence relation defined as follows: for any $B , C in cal(B)$ such that $x in B sect C$ and
  any $f in cal(F) lr((B))$, $g in cal(F) lr((C))$,
  $
    lr((B , f)) tilde.op lr((C , g)) arrow.l.r.double "there exists an element" D in cal(B) upright("such that") x in D subset.eq B sect C "and" f lr(|""_D = g|)_D .
  $

]



== Sheaf <sheaf>
=== Sheaf on Topological Space <sheaf-on-topological-space>
Let $lr((X , tau))$ be a topological space. Given a tuple of open sets $lr((U_i))_(i in I)$ or equivalently given a map $I arrow.r tau$,
we can define a preorder $lt.eq$ on $I times I$ by $ lr((i_1 , i_2)) lt.eq lr((j_1 , j_2)) arrow.l.r.double i_1 = j_1 = j_2 upright("or ") i_2 = j_1 = j_2 . $ Then $lr((I times I , lt.eq))$ can
be seen as a (0,1)-category, denoted as $mathsf(J)_I$. Define the diagram $K_I : mathsf(J)_I arrow.r mathsf("Open")_X$ as
follows:

#functor_diagram(
  F: $K_I$,
  C: $mathsf(J)_I$,
  D: $mathsf("Open")_X$,
  g: $$,
  X: $(i_1,i_2)$,
  Y: $(i_1,i_1)$,
  Fg: $iota$,
  FX: $U_(i_1) sect U_(i_2)$,
  FY: $U_(i_1)$,
)

#par(first-line-indent: 0pt)[
  We can check that $injlim K_I tilde.equiv union.big_(i in I) U_i$.
]


#definition[
  $mathsf(C)$-valued Sheaf on a Topological Space][
  Let $lr((X , tau))$ be a topological space and $mathsf(C)$ be a complete category. The #strong[category of $mathsf(C)$-valued sheaves on $X$], denoted as $mathsf(S h)_(mathsf(C)) lr((X))$, is a full subcategory of $mathsf(P S h)_(mathsf(C)) lr((X))$ defined as follows:

  - Objects: $mathsf(C)$-valued presheaves $cal(F)$ on $X$ such that one of the following equivalent condition holds:

    + #block[for any tuple of open sets $lr((U_i))_(i in I)$, $cal(F)$ preserves the limit of $K_I^(upright(o p))$, i.e., $cal(F)$ maps a colimit of $K_I$ to a limit of $cal(F) circle.stroked.tiny K_I^(upright(o p))$
        $
          cal(F)lr((injlim K_I)) = projlim cal(F) circle.stroked.tiny K_I^(upright(o p))
        $
        If we denote $U = union.big_(i in I) U_i$, then the limit cone of $cal(F) circle.stroked.tiny K_I^(upright(o p))$ is
        #commutative_diagram($
          &cal(F)(
            U
          ) edge("ld",op("res")_(U arrow.l.hook  U_(i_1)),->)edge("rd",op("res")_(U arrow.l.hook U_(i_1) sect thick U_(i_2)),"->")& \
          cal(F)(U_(i_1))edge("rr",op("res")_(U_(i_1)arrow.l.hook U_(i_1) sect thick U_(i_2)),->, #right)&&cal(F)(
            U_(i_1) sect thick U_(i_2)
          )
        $)
      ]

    + #block[for any open set $U subset.eq X$, for any open covering $U = union.big_(i in I) U_i$, the diagram
        $
          cal(F)(U)xarrow(width: #3em, "") product_(i in I ) cal(F)(
            U_i
          )xarrow(width: #3em, sym: arrows.rr, alpha_1)_(alpha_2)product_((i_1 , i_2) in I times I) cal(F) (
            U_(i_1) sect U_(i_2)
          )
        $
        is an equalizer diagram in the category $mathsf(C)$. Here $alpha_1$ and $alpha_2$ are the morphisms induced by the unversal property of product as follows:

        #square_cd_element(
          A11: ($cal(F)(U_(i_1))$, $$),
          A12: ($limits(product)_(i in I) cal(F)(U_i)$, $(f_i)_(i in I)$),
          A21: ($cal(F)(U_(i_1) sect U_(i_2))$, $$),
          A22: (
            $limits(product)_((i_1,i_2) in I times I) cal(F)(U_(i_1) sect U_(i_2))$,
            $(f_i_1|_(U_(i_1) sect U_(i_2)))_(i_1,i_2 in I)$,
          ),
          Ff: $pi_(i_1)$,
          Gf: $pi_(i_1,i_2)$,
          theta_l: ($op("res")_(U_(i_1) arrow.l.hook U_(i_1) sect U_(i_2))$, $$),
          theta_r: ($alpha_1$, $alpha_1$),
          Ff_arrow: "<-",
          Gf_arrow: "<-",
          theta_r_arrow: ("-->", "|->"),
        )

        #square_cd_element(
          A11: ($cal(F)(U_(i_2))$, $$),
          A12: ($limits(product)_(i in I) cal(F)(U_i)$, $(f_i)_(i in I)$),
          A21: ($cal(F)(U_(i_1) sect U_(i_2))$, $$),
          A22: (
            $limits(product)_((i_1,i_2) in I times I) cal(F)(U_(i_1) sect U_(i_2))$,
            $(f_i_1|_(U_(i_1) sect U_(i_2)))_(i_1,i_2 in I)$,
          ),
          Ff: $pi_(i_2)$,
          Gf: $pi_(i_1,i_2)$,
          theta_l: ($op("res")_(U_(i_2) arrow.l.hook U_(i_1) sect U_(i_2))$, $$),
          theta_r: ($alpha_2$, $alpha_2$),
          Ff_arrow: "<-",
          Gf_arrow: "<-",
          theta_r_arrow: ("-->", "|->"),
        )

        where $iota_1 : U_(i_1) sect U_(i_2) arrow.r.hook U_(i_1)$ and $iota_2 : U_(i_1) sect U_(i_2) arrow.r.hook U_(i_2)$ are the inclusion maps. When $mathsf(C) = mathsf("Set")$, $alpha_1$ and $alpha_2$ can be explicitly described as above.
      ]

  - Morphisms: A morphism of $mathsf(C)$-valued sheaves is a morphism of $mathsf(C)$-valued presheaves.

]


#proposition[
  Suppose $cal(F)$ is a $mathsf(C)$-valued presheaf on a topological space $X$.

  #block[
    #set enum(numbering: "(i)", start: 1)
    + $cal(F) lr((diameter))$ is terminal in $mathsf(C)$.
  ]

]


#proof[
  If we take $I = diameter$, the empty union $union.big_(i in diameter) U_i$ is the initial object $diameter$ in $mathsf(S e t)$, and the empty product $product_(i in diameter) cal(F) lr((U_i))$ is the terminal object $T$ in $mathsf(C)$. Therefore, the equalizer diagram above becomes
  $
    cal(F)(emptyset)xarrow(width: #3em, "") T xarrow(width: #3em, sym: arrows.rr, op("id")_T)_( op("id")_T)T
  $
  which means $cal(F) lr((diameter))$ is a terminal object in $mathsf(C)$.

]

#definition[
  Sheaf of Sets on a Topological Space
][
  Let $X$ be a topological space and $cal(F)$ is a presheaf of sets on $X$. $cal(F)$ is a #strong[sheaf of sets on $X$] if one of the following equivalent condition holds:

  + #block[#strong[Identity axiom]. If $U = union.big_(i in I) U_i$ is an open cover of an open set $U$, and $f , g in cal(F) lr((U))$ satisfy $ res(U , U_i) lr((f)) = res(U , U_i) lr((g)) upright("for all ") i in I , $ then $f = g$.

      #strong[Gluability axiom]. If $U = union.big_(i in I) U_i$ is an open cover of an open set $U$, and $lr((f_i))_(i in I) in product_(i in I) cal(F) lr((U_i))$ is a family of sections satisfying that $ res(U_i , U_i sect U_j) lr((f_i)) = res(U_j , U_i sect U_j) lr((f_j)) upright("for all ") i , j in I , $ then there exists $f in cal(F) lr((U))$ such that $res(U , U_i) lr((f)) = f_i$ for all $i in I$.]

  + For any open set $U subset.eq X$, any open cover $U = union.big_(i in I) U_i$ and any family of sections $lr((f_i))_(i in I) in product_(i in I) cal(F) lr((U_i))$ such that $ f_i\|_(U_i sect U_j) = f_j\|_(U_i sect U_j) upright("for all ") i , j in I , $ then there exists a unique section $f in cal(F) lr((U))$ such that $f_i = f\|_(U_i)$ for all $i in I$.

  + For any open set $U subset.eq X$, and any open covering $U = union.big_(i in I) U_i$, the diagram
    $
      cal(F)(U)xarrow(width: #3em, "") product_(i in I ) cal(F)(
        U_i
      )xarrow(width: #3em, sym: arrows.rr, j_1)_(j_2)product_((i_1 , i_2) in I times I) cal(F) (U_(i_1) sect U_(i_2))
    $
    is an equalizer diagram.
]
#proposition[
  Suppose the category $mathsf(C)$ and the functor $U : mathsf(C) arrow.r mathsf(S e t)$ have the following properties:

  + $U$ is faithful,

  + $mathsf(C)$ has limits and $U$ commutes with them,

  + the functor $U$ reflects isomorphisms.


  Let $X$ be a topological space and $cal(F)$ be a $mathsf(C)$-valued presheaf on $X$. Then $cal(F)$ is a $mathsf(C)$-valued sheaf if and only if the underlying $mathsf(S e t)$-valued presheaf $U circle.stroked.tiny cal(F)$ is a sheaf.

]
#corollary[
  Take $mathsf(C) = mathsf(G r p) , mathsf(R i n g) , R upright("-") mathsf(M o d) , R upright("-") mathsf(A l g) , 𝕜 upright("-") mathsf(V e c t)$ and $U$ to be the forgetful functor. Let $X$ be a topological space. Let $X$ be a topological space and $cal(F)$ be a $mathsf(C)$-valued presheaf on $X$. Then $cal(F)$ is a $mathsf(C)$-valued sheaf if and only if the underlying $mathsf(S e t)$-valued presheaf $U circle.stroked.tiny cal(F)$ is a sheaf.

]

#example[
  Constant Sheaf
][
  Let $X$ be a topological space and $A$ be a set. The #strong[constant sheaf with value $A$], denoted $underline(A)$, is the sheaf that assigns to an open set $U subset.eq X$ the set of all locally constant maps $U arrow.r A$ with restriction mappings given by restrictions of functions.

  #functor_diagram(
    F: $underline(A)$,
    C: $mathsf("Open")_X^(op("op"))$,
    D: $mathsf("Set")$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $U$,
    Fg: $res(V,U)=iota^*$,
    FX: ${f : V arrow.r A divides f "is locally constant"}$,
    FY: ${f : U arrow.r A divides f "is locally constant"}$,
  )
]<constant-sheaf>


#example[
  Pointwise Function Sheaf][
  Let $X$ be a topological space. Let $lr((A_x))_(x in X)$ be a family of sets $A_x$ indexed by points $x in X$. We can construct a $mathsf(S e t)$-valued sheaf $cal(F)$ as follows

  #functor_diagram(
    F: $cal(F)$,
    C: $mathsf("Open")_X^(op("op"))$,
    D: $mathsf("Set")$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $U$,
    Fg: $res(V,U)$,
    FX: $limits(product)_(x in V) A_x$,
    FY: $limits(product)_(x in U) A_x$,
    FX_e: $(a_x)_(x in V)$,
    FY_e: $(a_x)_(x in U)$,
  )
  where $res(V , U)$ is induced by the universal property of product. Now we check this is a sheaf.

  - Identity axiom: Suppose $U = union.big_(i in I) U_i$ is an open covering of an open set $U subset.eq X$. Let $s , s^prime in Pi lr((U))$ be two sections on $U$ such that $s\|_(U_i) = s^prime\|_(U_i)$ for all $i in I$. Assume $s = lr((a_x))_(x in U) , s^prime = lr((a_x^prime))_(x in U)$. Then we have $ forall i in I , #h(0em) forall x in U_i , #h(0em) a_x = a_x^prime arrow.r.double.long forall x in U , #h(0em) a_x = a_x^prime arrow.r.double.long s = s^prime . $

  - #block[Gluability axiom: Suppose $U = union.big_(i in I) U_i$ is an open covering of an open set $U subset.eq X$. Let
      $
        lr((s_i))_(i in I) = lr((lr((a_(i , x)))_(x in U_i)))_(i in I) in product_(i in I) cal(F) lr((U_i))
      $
      be a family of sections such that $s_i$ and $s_j$ agree over $U_i sect U_j$, i.e. $ a_(i , x) = a_(j , x) upright("in ") A_x upright("for all ") x in U_i sect U_j . $ Let $s = lr((a_x))_(x in X)$ where $a_x = a_(i , x)$ whenever $x in U_i$ for some $i$. Then $s\|_(U_i) = s_i$ for all $i$.]

]
#proposition[
  Section of a Sheaf is Determined by Its Germs at All Points
][
  Let $X$ be a topological space and $cal(F)$ be a $mathsf(S e t)$-valued sheaf on $X$. For any open set $U subset.eq X$, the map $iota$ induced by the universal property of product is injective.

  #commutative_diagram($
    cal(F)(
      U
    ) edge(iota, "-->") edge("dr", injlim_(x in U in tau), #right, "->") & limits(product)_(x in U) cal(F)_x edge("d", pi_x, #left, "->") \
    & cal(F)_x
  $)
]<section_is_determined_by_its_germs_at_all_points>
#proof[
  Suppose that $s , s^prime in cal(F) lr((U))$ such that $iota lr((s)) = iota lr((s^prime))$. Then for each $x in U$, $s , s^prime$ map to the same element in stalk $cal(F)_x$. This means that for every $x in U$, there exists an open $V^x subset.eq U , x in V^x$ such that $s\|_(V^x) = s^prime\|_(V^x)$. Note that $U = union.big_(x in U) V^x$ is an open covering. Thus by the uniqueness in the sheaf condition we see that $s = s^prime$.

]


#definition[
  Sheafification of a Presheaf][
  Let $X$ be a topological space and $cal(F)$ be a $mathsf(S e t)$-valued presheaf on $X$. The #strong[sheafification of $cal(F)$] is a sheaf $sheafify(F)$ on $X$ together with a presheaf morphism $phi : cal(F) arrow.r cal(F)^(" sh")$ such that for any sheaf $cal(G)$ and any morphism $psi : cal(F) arrow.r cal(G)$, there exists a unique morphism $psi^(+) : sheafify(F) arrow.r cal(G)$ such that the following diagram commutes:

  #commutative_diagram($
    cal(F) edge(phi, "-->") edge("dr",psi, #right, "->") &sheafify(F) edge("d", psi^+, #left, "->") \
    & cal(G)
  $)
  A construction of $sheafify(F)$ and $phi$ can be given as follows: for any open set $U subset.eq X$, we define the set of #strong[compatible germs] of $cal(F)$ on $U$ as $ sheafify(F) lr((U)) := lr(size:#115%,
    {(s_x)_(x in U) in product_(x in U) cal(F)_x thin mid(|) #box(baseline: 50%, $thin &"for any" x in U , "there exists an open neighborhood" x in V subset.eq U\
            &"and a section" f in cal(F)(V) "such that for all" y in V,  s_y = lr([lr((U , f))])_y$)}
  ) . $ If $U subset.eq V$, then the image of $sheafify(F) lr((V)) arrow.r.hook product_(x in V) cal(F)_x arrow.r product_(x in U) cal(F)_x$ still lies in $sheafify(F) lr((U))$, which gives a well-defined restriction map $res(V , U) : sheafify(F) lr((V)) arrow.r sheafify(F) lr((U))$. So we can define a presheaf of sets on $X$ as follows
  #functor_diagram(
    F: $sheafify(F)$,
    C: $mathsf("Open")_X^(op("op"))$,
    D: $mathsf("Set")$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $U$,
    Fg: $res(V,U)$,
    FX: $sheafify(F)(V)$,
    FY: $sheafify(F)(U)$,
  )

]

#example[Constant Sheaf is the Sheafification of Constant Presheaf][
  Let $X$ be a topological space and $A$ be a set. Let $cal(F)$ be the #link(<constant-presheaf>)[constant presheaf with value $A$]. Recall that if $cal(F)$ is a sheaf, there must be $cal(F)(diameter)={*}$. Therefore, if $A$ has more than one element, $cal(F)$ is not a sheaf. We can also check that $cal(F)$ violates the gluability axiom if $A={0,1}$ and $X={0,1}$ with the discrete topology. Take sections $0 in cal(F) lr(({0}))$ and $1 in cal(F) lr(({1}))$. We cannot find a section $f in cal(F) lr((X))$ such that $f\|_({0}) = 0$ and $f\|_({1}) = 1$.

  The sheafification of $cal(F)$ is the #link(<constant-sheaf>)[constant sheaf with value $A$]. That's because compatible germs of $cal(F)$ on an open set $U subset.eq X$ are just locally constant functions $f: U arrow.r A$.
]

#definition[
  Pushforward Sheaf
][
  Let $X$ be a topological space and $cal(F)$ be a sheaf on $X$. Let $f : X arrow.r Y$ be a continuous map. The #strong[pushforward sheaf] $f_(\*) cal(F)$ is the sheaf on $Y$ defined as follows:

  #functor_diagram(
    F: $f_* cal(F)$,
    C: $mathsf("Open")_X^(op("op"))$,
    D: $mathsf("Set")$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $U$,
    Fg: $res(V,U)$,
    FX: $sheafify(F)(V)$,
    FY: $sheafify(F)(U)$,
  )

  The #strong[pushforward sheaf functor] $f_(\*) : mathsf(S h)_(mathsf(C)) lr((X)) arrow.r mathsf(S h)_(mathsf(C)) lr((Y))$ is defined as follows:


]

#example[
  Skyscraper Sheaf
][
  Suppose $X$ is a topological space with $x in X$, and $S$ is a set. Let $i_x : { x } arrow.r X$ be the inclusion. Then we can define a functor $i_(x , *) S : mathsf("Open")_X^(upright(o p)) arrow.r mathsf("Set")$ as follows:

  - Map on objects:
  $
    i_(x , *) S lr((U)) = cases(
      delim: "{",
      S & upright("if ") x in U,
      , { * } & upright("if ") x in.not U .,

    )
  $

  - Map on morphisms: for any inclusion $iota : U arrow.r.hook V$, the image $op("res")_(V arrow.l.hook U) := i_(x , *) S lr((iota))$ is given by
  $
    op("res")_(V arrow.l.hook U) : i_(x , *) S lr((V)) & arrow.r.long i_(x , *) S lr((U))\
    s & arrow.r.bar.long cases(
      delim: "{",
      s & upright("if ") x in U,
      , { * } & upright("if ") x in.not U .,

    )
  $

  We can check that $i_(x , *) S$ is a sheaf on $X$, called the #strong[skyscraper sheaf] at $x$ with value $S$. It is the pushforward sheaf of the constant sheaf $underline(S)$ on ${ x }$ along the inclusion $i_x : { x } arrow.r.hook X$.

]

#example[
  Sheaf of Continuous Maps][
  Let $X$ and $Y$ be topological spaces. The #strong[sheaf of continuous maps from $X$ to $Y$], denoted $op("Hom")_mathsf("Top")(-,Y)$, is the sheaf on $X$ defined as follows:

  #functor_diagram(
    F: $op("Hom")_mathsf("Top")(-,Y)$,
    C: $mathsf("Open")_X^(op("op"))$,
    D: $mathsf("Set")$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $U$,
    Fg: $op("res")_(V arrow.l.hook U) = iota^*$,
    FX: $op("Hom")_mathsf("Top")(V,Y)$,
    FY: $op("Hom")_mathsf("Top")(U,Y)$,
  )

  If $Y$ is a topological space with discrete topology, then $op("Hom")_mathsf("Top")(-,Y)$ coincides with the constant sheaf $underline(Y)$.

]



#example[
  Sheaf of Continuous Sections of a Continuous Map][
  Let $E$ and $X$ be topological spaces and $p: E arrow.r X$ be a continuous map. The #strong[sheaf of sections of $p$], denoted $Gamma(-,p)$, is the sheaf on $X$ defined as follows:

  #functor_diagram(
    F: $Gamma(-,p)$,
    C: $mathsf("Open")_X^(op("op"))$,
    D: $mathsf("Set")$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $U$,
    Fg: $op("res")_(V arrow.l.hook U)=iota^*$,
    FX: $Gamma(V,p)$,
    FY: $Gamma(U,p)$,
  )
  where
  $
    Gamma(U,p)={s : U arrow.r f^(- 1) (U) mid(|) s "is a continous map such that" f circle.stroked.tiny s = op("id")_U}
  $
  consists of local continuous sections of $p:E arrow X$ over $U subset.eq X$.

  The construction of $Gamma(-,p)$ is functorial in $p$. In other words, we can define a functor $Gamma : mathsf("Top")\/X arrow.r mathsf("Sh")_mathsf("Set")(X) arrow.r.hook mathsf("PSh")_mathsf("Set")(X)$ as follows:

  #functor_diagram(
    F: $Gamma$,
    C: $mathsf("Top")\/X$,
    D: $mathsf("PSh")_mathsf("Set")(X)$,
    g: $g$,
    X: $Y_1 xarrow(p_1) X$,
    Y: $Y_2 xarrow(p_2, position: #bottom) X$,
    Fg: $g_*$,
    FX: $Gamma(-,p_1)$,
    FY: $Gamma(-,p_2)$,
    Fg_arrow: "=>",
    FX_e: $s$,
    FY_e: $g circle.tiny s$,
  )

]

#definition[
  Étale Space][
  An #strong[étale map] over a topological space $X$ is an object $p : E arrow.r X$ in slice category $mathsf("Top")\/ X$ such that $p$ is a local homeomorphism. The topological space $E$ is called an #strong[étale space] over $X$. The set $E_x = p^(- 1) lr((x))$ where $x in X$ is called the #strong[stalk] of $p$ over $x$. The category of étale maps over $X$ is the full subcategory of $mathsf("Top")\/ X$ consisting of étale spaces over $X$, denoted $mathsf("Et")_X$.

]
#definition[
  Étale Space of a Presheaf][ Let $X$ be a topological space and $cal(F)$ be a presheaf on $X$. The #strong[étale space] of $cal(F)$, denoted $op("Et")lr((cal(F)))$, is defined as follows:

  - The underlying set of $op("Et")lr((cal(F)))$ is
  $ product.co_(x in X) cal(F)_x = lr({lr((x , s)) thin | thin x in X , s in cal(F)_x}) . $

  - The topology on $op("Et")lr((cal(F)))$ is generated by the following basis:
  $
    lr(
      {lr({lr((x , s)) in product.co_(x in X) cal(F)_x mid(|) x in U}) mid(|) U upright("is open in ") X}
    ) .
  $

  Define a map $p : op("Et")lr((cal(F))) arrow.r X$ by $p lr((x , s)) = x$. Then $p$ is a local homeomorphism. Therefore, $p : op("Et")r((cal(F))) arrow.r X$ is an étale map over $X$, which justifies the notation.

  This construction is functorial. In other words, we can define a functor $op("Et"): mathsf("PSh")_(mathsf("Set")) lr((X)) arrow.r mathsf("Et")_X arrow.r.hook mathsf("Top")\/ X$ as follows:


  #functor_diagram(
    F: $op("Et")$,
    C: $mathsf("PSh")_mathsf("Set")(X)$,
    D: $mathsf("Top")\/X$,
    g: $phi$,
    X: $cal(F)$,
    Y: $cal(G)$,
    Fg: $op("Et")(phi)$,
    FX: $op("Et")(cal(F)) xarrow(p_1) X$,
    FY: $op("Et")(cal(G)) xarrow(p_2, position: #bottom) X$,
    g_arrow: "=>",
    FX_e: $(x,s)$,
    FY_e: $(x,phi(s))$,
  )

  We have the following adjunction: $ "Hom"_(mathsf("Top")\/ X) lr((mathsf("Et")lr((-)) , -)) tilde.equiv "Hom"_(mathsf("PSh")_(mathsf("Set")) lr((X))) lr((- , Gamma lr((-)))) . $ Furthermore, this adjunction restricts to an equivalence of categories

  #commutative_diagram({
    let (A, B) = ((0, 0), (1, 0))
    node(A, $mathsf("Sh")_(mathsf("Set"))(X)$)
    node(B, $mathsf("Et")_X$)
    edge(A, B, $"Et"$, "->", bend: +35deg)
    edge(A, B, $Gamma$, "<-", bend: -35deg)
  })
]


=== Sheaf on Topological Base
<sheaf-on-a-base-for-topology-space>
#definition[
  $mathsf("Set")$-valued Sheaves on a Topological Base][
  Let $X$ be a topological space. Let $cal(B)$ be a basis for the topology on $X$. A $mathsf("Set")$-valued sheaf on $cal(B)$ is a $mathsf("Set")$-valued presheaf on $cal(B)$ which satisfies the one of the following equivalent conditions:


  + #strong[Identity axiom]. If $B in cal(B)$ is a basic open set, $B = union.big_(i in I) B_i$ is a cover of a $B$ with $B_i in cal(B)$, and $f , g in cal(F) lr((B))$ satisfy $ op("res")_(B arrow.l.hook B_i) lr((f)) = op("res")_(B arrow.l.hook B_i) lr((g)) upright("for all ") i in I , $ then $f = g$. \
    #strong[Gluability axiom]. If $B in cal(B)$ is a basic open set, $B = union.big_(i in I) B_i$ is a cover of a $B$ with $B_i in cal(B)$, and $lr((f_i))_(i in I) in product_(i in I) cal(F) lr((B_i))$ is a family of sections satisfying that for any $i , j in I$ and any basic open set $V subset.eq B_i sect B_j$, $ op("res")_(B_i arrow.l.hook V) lr((f_i)) = op("res")_(B_j arrow.l.hook V) lr((f_j)) , $ then there exists $f in cal(F) lr((B))$ such that $op("res")_(B arrow.l.hook B_i) lr((f)) = f_i$ for all $i in I$.

  + If $B in cal(B)$ is a basic open set, $B = union.big_(i in I) B_i$ is a cover of a $B$ with $B_i in cal(B)$, $B_i sect B_j = union.big_(k in I_(i j)) V_k^(i j)$ is a cover of $B_i sect B_j$ with $V_k^(i j) in cal(B)$, and $lr((f_i))_(i in I) in product_(i in I) cal(F) lr((B_i))$ is a family of sections satisfying that $forall i , j in I , forall k in I_(i j)$, $ f_i\|_(V_k^(i j)) = f_j\|_(V_k^(i j)) , $ there exists a unique section $f in cal(F) lr((B))$ such that $f_i = f\|_(B_i)$ for all $i in I$.
]


#definition[
  $mathsf(C)$-valued Sheaves on a Topological Base][Let $X$ be a topological space and $mathsf(C)$ be a complete category. Let $cal(B)$ be a basis for the topology on $X$. A $mathsf(C)$-valued sheaf on $cal(B)$ is a $mathsf(C)$-valued presheaf on $cal(B)$ which satisfies the one of the following equivalent conditions: If $B in cal(B)$ is a basic open set, $B = union.big_(i in I) B_i$ is a cover of a $B$ with $B_i in cal(B)$, and $B_i sect B_j = union.big_(k in I_(i j)) V_k^(i j)$ is a cover of $B_i sect B_j$ with $V_k^(i j) in cal(B)$, then the diagram
  $
    cal(F)(B)xarrow(width: #3em, "") product_(i in I ) cal(F)(
      B_i
    )xarrow(width: #3em, sym: arrows.rr, alpha_1)_(alpha_2) product_((
      i , j
    ) in I times I) product_(k in I_(i j)) cal(F) (V_k^(i j))
  $
  is an equalizer diagram in the category $mathsf(C)$.

]
#definition[
  Category of $mathsf("Set")$-valued Sheaves on a Topological Base][
  Let $X$ be a topological space. Let $cal(B)$ be a basis for the topology on $X$. The #strong[category of $mathsf("Set")$-valued sheaves on $cal(B)$], denoted as $mathsf("Sh")_(mathsf("Set")) lr((cal(B)))$, is defined as the full subcategory of $mathsf(P S h)_(mathsf("Set")) lr((cal(B)))$ consisting of $mathsf("Set")$-valued sheaves on $cal(B)$.

]
#proposition[
  Extend Sheaf from a Basis to a Topological Space][
  Let $X$ be a topological space. Let $cal(B)$ be a basis for the topology on $X$.

  #block[
    #set enum(numbering: "(i)", start: 1)
    + #block[If $tildecal(F)$ is a $mathsf("Set")$-valued sheaf on $cal(B)$, then it extends uniquely to a $mathsf("Set")$-valued sheaf $cal(F)$ on $X$ by
        $
          cal(F) (U) & := projlim_(V in cal(B) , V subset.eq U ) tildecal(F) (V)\
          & = {
            (f_V) in product_(V in cal(B) , V subset.eq U) tildecal(F) (V) mid(|) "res"_(V arrow.l.hook W) (
              f_V
            ) = f_W "for any" V , W in cal(B) "such that " W subset.eq V subset.eq U
          }
        $]

    + Given sheaves $cal(F)$ and $cal(G)$ on $X$ and a collection of maps $ tilde(phi) lr((U)) : cal(F) lr((U)) --> cal(G) lr((U)) upright("for all ") U in cal(B) $ commuting with restrictions, there is a unique morphism $phi : cal(F) arrow.r cal(G)$ of sheaves such that $phi lr((U)) = tilde(phi) lr((U))$ for all $U in cal(B)$.
  ]
]

#proposition[Extended Sheaf has the Same Stalks][
  Let $X$ be a topological space. Let $cal(B)$ be a basis for the topology on $X$. Let $tildecal(F)$ be a $mathsf("Set")$-valued sheaf on $cal(B)$ and $cal(F)$ be the extension of $tildecal(F)$ to $X$. Then for any $x in X$, the stalk $cal(F)_x$ is isomorphic to the stalk $tildecal(F)_x$.
]<extended-sheaf-has-the-same-stalks>
#proof[
  Consider the composition of functors $cal(F)|_(mathsf(B)_(x)^(op("op")))=cal(F)|_(mathsf("Open")_(X , x)^(op("op")))circle.tiny iota^(op("op"))$
  #commutative_diagram(
    spacing: 4em,
    $
      mathsf(B)_(x)^(op("op")) edge("r", iota^(op("op")), "hook->") & mathsf("Open")_(X , x)^(op("op")) edge("r", cal(F)|_(mathsf("Open")_(X , x)^(op("op"))), ->) &mathsf("Set")
    $,
  )
  Since $iota^(op("op"))$ is a final functor, we have
  $
    tildecal(F)_x = injlim cal(F)|_(mathsf(B)_(x)^(op("op"))) = injlim cal(F)|_(mathsf("Open")_(X , x)^(op("op")))circle.tiny iota^(op("op")) tilde.equiv injlim cal(F)|_(mathsf("Open")_(X , x)^(op("op"))) = cal(F)_x
  $.
]

== Ringed Space <ringed-space>
#definition[
  Ringed Spaces][
  A #strong[ringed space] is a pair $lr((X , cal(O)_X))$, where $X$ is a topological space and $cal(O)_X$ is a sheaf of commutative rings on $X$.

]
#definition[
  Category of Ringed Spaces][
  The #strong[category of ringed spaces] consists of the following data:

  - Objects: ringed spaces $lr((X , cal(O)_X))$.

  - Morphisms: morphisms of ringed spaces $lr((f , f^(♯))) : lr((X , cal(O)_X)) arrow.r lr((Y , cal(O)_Y))$, where $f : X arrow.r Y$ is a continuous map and $f^(♯) : cal(O)_Y arrow.r f_(\*) cal(O)_X$ is a morphism of sheaves on $Y$.

    #square_cd(
      A11: $cal(O)_Y (V)$,
      A12: $cal(O)_Y (U)$,
      A21: $cal(O)_X (f^(-1)(V))$,
      A22: $cal(O)_X (f^(-1)(U))$,
      Ff: $op("res")_(V arrow.l.hook U)$,
      Gf: $op("res")_(V arrow.l.hook U)$,
      theta_l: $f_(V)^♯$,
      theta_r: $f_(U)^♯$,
    )
]
#definition[
  Locally Ringed Spaces][A ringed space $lr((X , cal(O)_X))$ is called a #strong[locally ringed space] if for every $x in X$, the stalk $cal(O)_(X , x)$ is a local ring.

]<locally-ringed-space>
#definition[
  Morphism of Locally Ringed Spaces
][
  A morphism of locally ringed spaces $lr((f , f^(♯))) : lr((X , cal(O)_X)) arrow.r lr((Y , cal(O)_Y))$ is a morphism of ringed spaces such that for every $x in X$, the induced map $f_x^(♯) : cal(O)_(Y , f lr((x))) arrow.r cal(O)_(X , x)$ is a local ring homomorphism.

]
#definition[
  Residue Field
][
  Let $lr((X , cal(O)_X))$ be a locally ringed space and $x in X$. The #strong[residue field] of $X$ at $x$ is the field $cal(O)_(X , x) \/ frak(m)_(X , x)$, where $frak(m)_(X , x)$ is the maximal ideal of $cal(O)_(X , x)$.

]<residue-field>

#pagebreak()

= Scheme <scheme>
== Affine Scheme <affine-scheme>
Affine schemes are the basic building blocks of schemes. They are locally ringed spaces consisting of underlying sets, topologies, and sheaves of rings. We will descibe them in detail in this section.

=== Underlying Set of $op("Spec")(R)$ <undering-set-of-mathopmathrmspecleftrright>
#definition[
  Spectrum of a Commutative Ring][
  The #strong[spectrum] of a commutative ring $R$ is the set of all prime ideals of $R$, and is usually denoted by $op("Spec")(R)$.

]
#proposition[
  According to the knowledge of commutative rings, we have the following facts.

  + If $R$ is an integral domain, then $lr((0)) in op("Spec")(R)$.

  + If $R$ is a field, then $op("Spec")(R) = { lr((0)) }$.

  + If $R = 0$ is the zero ring, then $op("Spec")(R) = diameter$.

]
#example[
  $op("Spec") lr((bb(Z)))$][
  The prime ideals of $bb(Z)$ are $lr((0))$ and $lr((p))$, where $p$ is a prime number. So we have $ op("Spec") lr((bb(Z))) = { lr((0)) , lr((2)) , lr((3)) , lr((5)) , lr((7)) , lr((11)) , dots.h.c } $

]
#example[
  Affine $n$-space $affine_R^n$][
  Let $R$ be a commutative ring. The #strong[affine $n$-space] over $R$ is $op("Spec")(R) lr([x_1 , x_2 , dots.h.c , x_n])$, denoted by $affine_(R)^n$.

]
#example[
  $affine_𝕜^1 = op("Spec") lr((𝕜 lr([x])))$][
  The affine line over a field $𝕜$ is given by $ op("Spec") lr((𝕜 lr([x]))) = { lr((0)) } union lr({lr((f)) divides f upright("is irreducible over ") 𝕜 lr([x])}) . $ Note $𝕜 lr([x])$ is an Euclidean domain. We see $op("Spec") lr((𝕜 lr([x])))$ has infinitely many elements, and each element corresponds to a maximal ideal of $𝕜 lr([x])$. \
  Geometrically, describing a irreducible polynomial $f in 𝕜 lr([x])$ is equivalent to specifying the set of all of its roots over $𝕜^(op("sep")) lr([x])$. Thus closed points of $affine_𝕜^1$ can be seen as an orbit space of the action of the absolute Galois group $op("Gal")lr((𝕜^(op("sep")) \/ 𝕜))$ on $𝕜^(op("sep"))$.

]
#example[
  $affine_(bb(C))^1 = op("Spec") lr((bb(C) lr([x])))$][
  The complex affine line $affine_(bb(C))^1$ is given by $ op("Spec") lr((bb(C) lr([x]))) = { lr((0)) } union { lr((x - a)) divides a in bb(C) } . $

]
#example[
  $affine_(overline(𝕜))^1 = op("Spec") lr((overline(𝕜) lr([x])))$][
  If $overline(𝕜)$ is an algebraically closed field, then the affine line $affine_(overline(𝕜))^1$ is given by $ op("Spec") lr((overline(𝕜) lr([x]))) = lr({lr((0))}) union lr({lr((x - a)) divides a in overline(𝕜)}) . $

]
#example[
  $affine_(bb(R))^1 = op("Spec") lr((bb(R) lr([x])))$][
  The real affine line $affine_(bb(R))^1$ is given by $ op("Spec") lr((bb(R) lr([x]))) = { lr((0)) } union { lr((x - a)) divides a in bb(R) } union lr({lr((x^2 + b x + c)) divides b , c in bb(R) , #h(0em) b^2 - 4 c < 0}) . $

]
#example[
  $affine_(bb(C))^2 = op("Spec") lr((bb(C) lr([x , y])))$][
  The complex affine plane $affine_(bb(C))^2$ is given by $ op("Spec") lr((bb(C) lr([x , y]))) = { lr((0)) } union { lr((x - a , y - b)) divides a , b in bb(C) } union lr({lr((f)) divides f upright("is irreducible over ") bb(C) lr([x , y])}) . $

]
#example[
  $affine_(bb(C))^n = op("Spec") lr((bb(C) lr([x_1 , dots.h.c , x_n])))$][
  The prime ideals of $bb(C) lr([x , y])$ includes $lr((0))$, $lr((x_1 - a_1, dots.c , x_n - a_n))$ and $lr((f))$, where $a_i in bb(C)$ and $f$ is an irreducible polynomial in $bb(C) lr([x_1 , dots.h.c , x_n])$. \
  By the Hilbert's Nullstellensatz, $lr((x_1 - a_1 , dots.h.c , x_n - a_n))$ are exactly all maximal ideals of $bb(C) lr([x_1 , dots.h.c , x_n])$. Therefore, we have a bijection between closed points of $affine_(bb(C))^n$ and $bb(C)^n$.

]
Given en element $f$ of a commutative ring $R$, we can evaluate $f$ at a prime ideal $lr([frak(p)]) in op("Spec")(R)$ by defining $f lr((lr([frak(p)])))$ to be the image of $f$ under the projection $pi : R arrow.r R \/ frak(p)$, that is $f lr((lr([frak(p)]))) = f + frak(p)$.

To get an intuition, we can consider a polynomial $f in bb(C) lr([x])$, and a prime ideal $frak(p) = lr((x - a))$, then $f lr((lr([frak(p)]))) = f lr((x)) + lr((x - a)) = f lr((a))$. In this case, we see $f lr((a)) = 0 arrow.l.r.double f in lr((x - a))$. Generally, we should make $f$ vanish by modding out by ideals, rather than through evaluation. So we have $f lr((lr([frak(p)]))) = 0 arrow.l.r.double f in frak(p)$, which is the same as saying $f$ vanishes at $frak(p)$. This motivates the following definition.

#definition[
  Vanishing Set][
  Given a subset $S$ of a commutative ring $R$, the #strong[vanishing set] of $S$ is defined as follows: $ V lr((S)) = lr({lr([frak(p)]) in op("Spec")(R) divides S subset.eq frak(p)}) . $ In particular, if $S = { f }$, then we write $V lr((f))$ instead of $V lr(({ f }))$ and call it the #strong[vanishing set of $f$] $ V lr((f)) = lr({lr([frak(p)]) in op("Spec")(R) divides f in frak(p)}) . $ $V lr((dot.op))$ can be seen as a map from the power set of $R$ to the power set of $op("Spec")(R)$, that is
  $
    V : 2^R & --> 2^(thin op("Spec")(R))\
    S & arrow.r.bar.long lr({lr([frak(p)]) in op("Spec")(R) thin | thin S subset.eq frak(p)}) .
  $

]
Every $f in S$ vanishes at $lr([frak(p)])$ is equivalent to $S subset.eq frak(p)$, since $frak(p)$ will be modded out to become $0$ in $R \/ frak(p)$. In a similar way, we can define the non-vanishing set to be the complement of the vanishing set as follows.

#definition[
  Non-vanishing Set][
  Given a subset $S$ of a commutative ring $R$, the #strong[non-vanishing set] of $S$ is defined as follows: $ D lr((S)) = op("Spec")(R) - V lr((S)) = lr({lr([frak(p)]) in op("Spec")(R) divides S subset.eq.not frak(p)}) . $ In particular, if $S = { f }$, then we write $D lr((f))$ instead of $D lr(({ f }))$ and call it the #strong[non-vanishing set of $f$] $ D lr((f)) = op("Spec")(R) - V lr((f)) = { lr([frak(p)]) in op("Spec")(R) divides f in.not frak(p) } . $ $D lr((dot.op))$ can be seen as a map from the power set of $R$ to the power set of $op("Spec")(R)$, that is $ D : 2^R & arrow.r 2^(thin op("Spec")(R))\
  S       & arrow.r.bar { lr([frak(p)]) in op("Spec")(R) divides S subset.eq.not frak(p) } . $

]
=== Topology on $op("Spec")(R)$<topology-on-mathopmathrmspecleftrright>
==== Zariski Topology <zariski-topology>
#definition[
  Zariski Topology][
  Given a commutative ring $R$, the #strong[Zariski topology] on $op("Spec")(R)$ is defined by taking the collection of all vanishing sets as the closed sets, that is, $ upright("Collection of closed sets") = lr({V lr((S)) in 2^(thin op("Spec")(R)) thin | thin S subset.eq R}) . $ Or equivalently, Zariski topology can be defined by taking the collection of all non-vanishing sets as the open sets, that is, $ upright("Collection of open sets") = lr({D lr((S)) in 2^(thin op("Spec")(R)) thin | thin S subset.eq R}) . $

]
#proposition[
  Properties of $V$
][
  Suppose $R$ is a commutative ring. Then the vanishing set function $V : 2^R arrow.r 2^(thin op("Spec")(R))$ satisfies the following properties:

  + $V$ is inclusion reversing, that is, if $S_1 subset.eq S_2 subset.eq R$, then $V lr((S_2)) subset.eq V lr((S_1))$.

  + $V lr((S)) = V lr((lr((S)))) = V lr((sqrt(lr((S)))))$ for any subset $S$ of $R$. Specially, $V lr((f)) = V lr((f^n))$.

  + $V lr((0)) = V lr((sqrt(0))) = op("Spec")(R)$ and $V lr((1)) = V lr((R)) = diameter$.

  + Let $frak(a)$ be an ideal in $R$. Then $V lr((frak(a))) = diameter$ if and only if $frak(a) = R$.

  + Let $frak(a)$ and $frak(b)$ be two ideals in $R$. Then $ V lr((frak(a) sect frak(b))) = V lr((frak(a) frak(b))) = V lr((frak(a))) union V lr((frak(b))) . $

  + Let $lr({frak(a)_i})_(i in I)$ be a family if ideals in $A$. Then $ V lr((sum_(i in I) frak(a)_i)) = sect.big_(i in I) V lr((frak(a)_i)) . $

  + Let $S$ be a subset of $R$. Then $ V lr((S)) = sect.big_(f in S) V lr((f)) . $


]
#proof[
  #block[
    #set enum(numbering: "(i)", start: 1)
    + If $S_1 subset.eq S_2 subset.eq R$, then we have $ in V lr((S_2)) & arrow.r.double.long S_2 subset.eq frak(p) arrow.r.double.long S_1 subset.eq frak(p) arrow.r.double.long lr([frak(p)]) in V lr((S_1)) , $ which means $V lr((S_2)) subset.eq V lr((S_1))$.

    + Since $sqrt(lr((S)))$ is the intersection of all prime ideals containing $S$, we have $ lr([frak(p)]) in V lr((S)) & arrow.l.r.double S subset.eq frak(p) arrow.l.r.double sqrt(lr((S))) subset.eq frak(p) arrow.l.r.double lr([frak(p)]) in V lr((sqrt(lr((S))))) , $ which means $V lr((S)) = V lr((sqrt(lr((S)))))$. Note that $sqrt(lr((f^n))) = sqrt(lr((f))^n) = sqrt(lr((f)))$, we have $V lr((f)) = V lr((f^n))$.

    + $
        V lr((0)) = lr({lr([frak(p)]) in op("Spec")(R) divides 0 in frak(p)}) = op("Spec")(R) .
      $ Since $lr((1)) = R$, we have $ V lr((1)) = V lr((R)) = lr({lr([frak(p)]) in op("Spec")(R) divides R subset.eq frak(p)}) = diameter . $

    + If $frak(a)$ is a ideal in $R$, and $V lr((frak(a))) = diameter$, then $frak(a)$ is not contained in prime ideals. Note maximal ideals are prime ideals. So $frak(a)$ is not contained in maximal ideals, which means $frak(a) = R$.

    + $
        lr([frak(p)]) in V lr((frak(a) sect frak(b))) & arrow.l.r.double frak(a) sect frak(b) subset.eq frak(p)\
        lr([frak(p)]) in V lr((frak(a) frak(b))) & arrow.l.r.double frak(a) frak(b) subset.eq frak(p)\
        & arrow.l.r.double frak(a) subset.eq frak(p) upright("or ") frak(b) subset.eq frak(p)\
        & arrow.l.r.double lr([frak(p)]) in V lr((frak(a))) upright("or ") lr([frak(p)]) in V lr((frak(b)))\
        & arrow.l.r.double lr([frak(p)]) in V lr((frak(a))) union V lr((frak(b))) .
      $

    + $
        lr([frak(p)]) in V lr((sum_(i in I) frak(a)_i)) & arrow.l.r.double sum_(i in I) frak(a)_i subset.eq frak(p)\
        & arrow.l.r.double frak(a)_i subset.eq frak(p) upright("for all ") i in I\
        & arrow.l.r.double lr([frak(p)]) in V lr((frak(a)_i)) upright("for all ") i in I\
        & arrow.l.r.double lr([frak(p)]) in sect.big_(i in I) V lr((frak(a)_i)) .
      $

    + $
        V lr((S)) = V lr((lr((S)))) = V lr((sum_(f in S) lr((f)))) = sect.big_(f in S) V lr((lr((f)))) = sect.big_(f in S) V lr((f)) .
      $
  ]

]
#proposition[
  Properties of $D$][
  Suppose $R$ is a commutative ring. Then the non-vanishing set function $D : 2^R arrow.r 2^(thin op("Spec")(R))$ satisfies the following properties:

  #block[
    #set enum(numbering: "(i)", start: 1)
    + $D$ is inclusion preserving, that is, if $S_1 subset.eq S_2 subset.eq R$, then $D lr((S_1)) subset.eq D lr((S_2))$.

    + $D lr((S)) = D lr((lr((S)))) = D lr((sqrt(lr((S)))))$ for any subset $S$ of $R$. Specially, $D lr((f)) = D lr((f^n))$.

    + $D lr((0)) = D lr((sqrt(0))) = diameter$ and $D lr((1)) = D lr((R)) = op("Spec")(R)$.

    + Let $frak(a)$ and $frak(b)$ be two ideals in $R$. Then $ D lr((frak(a))) sect D lr((frak(b))) = D lr((frak(a) frak(b))) . $

    + Let $lr({frak(a)_i})_(i in I)$ be a family if ideals in $A$. Then $ union.big_(i in I) D lr((frak(a)_i)) = D lr((sum_(i in I) frak(a)_i)) . $
  ]

]

#definition[
  $I (Y)$ for $Y subset.eq op("Spec")(R)$
][
  Let $R$ be a
  commutative ring. The map $I$ assigns to each subset $Y$ of $op("Spec")(R)$
  the ideal $I (Y)$ of $R$ defined by
  $
    I : 2^(upright(S p e c) (R)) & arrow.r 2^R\
    Y & arrow.r sect.big_([frak(p)] in Y) frak(p)
  $
]

#proposition[Properties of $I$][
  Suppose $R$ is a commutative ring.
  Then the non-vanishing set function $I : 2^(op("Spec")(R)) arrow.r 2^R$
  satisfies the following properties:

  + $I$ is inclusion-reversing: if $Y_1 subset.eq Y_2 subset.eq op("Spec")  (R)$, then $I (Y_2) subset.eq I (Y_1)$.

  + $I (Y_1 union Y_2) = I (Y_1) sect I (Y_2)$ for any subsets $Y_1 , Y_2$ of $op("Spec") (R)$.

  + For any ideal $frak(a) subset.eq R , I (V (frak(a))) = sqrt(frak(a))$.

  + For any subset $Y subset.eq op("Spec") (R) , I (Y) = I (overline(Y)) , V (I (Y)) = overline(Y)$.
]

#theorem[
  Hilbert's Nullstellensatz][
  Let $upright(R a d) lr((R))$ be the collection of all radical ideals of $R$ and $ mono(C l o s e d)_(op("Spec")(R)) := lr({A subset.eq op("Spec")(R) thin | thin A upright("is closed")}) $ be the collection of closed subsets of $op("Spec")(R)$. Then by restricting $V : 2^R arrow.r 2^(thin op("Spec")(R))$ to $upright(R a d) lr((R))$, we obtain the following bijection:
  $
    V : "Rad" (R) & arrow.r^tilde.op upright("Closed ")_(op("Spec") (R))\
    frak(a) & arrow.r.bar V (frak(a)) .
  $
  The inverse map of $V\|_(upright(R a d) lr((R)))$ is $I\|_(mono(C l o s e d)_(op("Spec")(R)))$. Furthermore, $V$ is an order isomorphism between the partial order sets $lr((upright(R a d) lr((R)) , subset.eq))$ and $lr((mono(C l o s e d)_(op("Spec")(R)) , supset.eq))$.

]

#corollary[
  Algebra-Geometry Dictionary][
  $V$ and $I$ are mutually inverse bijections when restricting to
  $
    V : op("Spec")(R) & arrow.long.r^tilde.op mono("Closed_Irreducible")\
    frak(p) & arrow.long.r.bar V (frak(p)) = overline({ frak(p) })
  $
  or
  $
    V : "MinPrime" (R) &arrow.long.r^tilde.op mono("Irreducible_Component")_(op("Spec") (R))\
    frak(q)& arrow.long.r.bar V (frak(q)) = overline({ frak(q) })\
  $ or
  $
    V : "Max" (R)& arrow.long.r^tilde.op mono("Closed_Singleton")_(op("Spec") (R))\
    frak(m) & arrow.long.r.bar V (frak(m)) = {frak(m)}\
  $

]
#proposition[
  Base of Zariski Topology][
  The collection of all sets of the form $D lr((f))$, where $f in R$, $ lr({D lr((f)) in 2^(thin op("Spec")(R)) thin | thin f in R}) $ forms a base for the Zariski topology on $op("Spec")(R)$. For this reason, we call the sets $D lr((f))$ #strong[distinguished open set]. They form a full subcategory of the category of $mathsf(O p e n)_(op("Spec")(R))$. We denote this category as $mathsf(B Z a r)_R$.

]<base_of_zariski_topology>
#proof[
  For any open set $D lr((S))$ of $op("Spec")(R)$, we have $ D lr((S)) & = op("Spec")(R) - V lr((S))\
            & = op("Spec")(R) - sect.big_(f in S) V lr((f))\
            & = union.big_(f in S) lr((op("Spec")(R) - V lr((f))))\
            & = union.big_(f in S) D lr((f)) . $

]
#proposition[
  Suppose $R$ is a commutative ring and $frak(a)$ be an ideal of $R$. Then $f in R$ vanishes on $V lr((frak(a)))$ $arrow.l.r.double$ $V lr((frak(a))) subset.eq V lr((f))$ $arrow.l.r.double$ $f^n in frak(a)$ for some $n gt.eq 1$.
]<vanish_on_v>
#proof[
  $
    f in R upright("vanishes on ") V lr((frak(a))) & arrow.l.r.double forall lr([frak(p)]) in V lr((frak(a))) , f in frak(p)\
    & arrow.l.r.double f in sect.big_(lr([frak(p)]) in V lr((frak(a)))) frak(p)\
    & arrow.l.r.double f in sect.big_(lr([frak(p)]) in op("Spec")(R)\
    frak(a) subset.eq frak(p)) frak(p) = sqrt(frak(a))\
    & arrow.l.r.double f^n in frak(a) upright("for some ") n gt.eq 1\
    & arrow.l.r.double lr((f)) subset.eq sqrt(frak(a))\
    & arrow.l.r.double V lr((frak(a))) subset.eq V lr((f)) .
  $

]
#corollary[
  If $g_1 , dots.h.c , g_m in R$, then $D lr((f)) subset.eq union.big_(i = 1)^m D lr((g_i))$ if and only if $g_1 / 1 , dots.h.c , g_m / 1$ generate $R_f$ in $R_f$.

]<distinguished_open_sets_inclusion_implies_generating_unit_ideal>
#proof[
  Denote $frak(a) = lr((g_1 , dots.h.c , g_n))$. Then by we have $ D lr((f)) subset.eq union.big_(i = 1)^n D lr((g_i)) & arrow.l.r.double D lr((f)) subset.eq D lr((frak(a))) arrow.l.r.double V lr((frak(a))) subset.eq V lr((f))\
                                                      & arrow.l.r.double f^n in frak(a) upright("for some ") n gt.eq 1 arrow.l.r.double f^n = sum_(i = 1)^m a_i g_i upright("for ") a_i in R\
                                                      & arrow.l.r.double 1 = sum_(i = 1)^m a_i / f^n g_i / 1 arrow.l.r.double lr((g_1 / 1 , g_2 / 1 , dots.h.c , g_m / 1)) = R_f . $

]
#proposition[
  Functor $op("Spec"): mathsf("CRing")^(op("op"))-> mathsf("Top")$ ][

  The map $op("Spec")$ is a contravariant functor from the category of commutative rings to the category of topological space.

  #functor_diagram(
    F: $op("Spec")$,
    C: $mathsf("CRing")^(op("op"))$,
    D: $mathsf("Top")$,
    g: $phi$,
    X: $R$,
    Y: $S$,
    Fg: $op("Spec")(phi)$,
    FX: $op("Spec")(R)$,
    FY: $op("Spec")(S)$,
    Fg_e: $phi^(-1)$,
    FX_e: $[phi^(- 1) (frak(p))]$,
    FY_e: $[frak(p)]$,
    contravariant: true,
  )

]
#proof[
  First, $phi^(- 1)$ is a well-defined map from $op("Spec") lr((S))$ to $op("Spec")(R)$ since $phi^(- 1)$ maps any prime ideal of $S$ to a prime ideal of $R$. Then we show that $phi^(- 1)$ is continuous. Let $V lr((frak(a)))$ be a closed subset of $op("Spec")(R)$. Then we can check that $ lr(("Spec" lr((phi))))^(- 1) lr((V lr((frak(a))))) & = lr(
    {lr([frak(p)]) in op("Spec") lr((S)) thin | thin phi^(- 1) lr((lr([frak(p)]))) in V lr((frak(a)))}
  )\
                                                     & = lr(
    {lr([frak(p)]) in op("Spec") lr((S)) thin | thin lr([phi^(- 1) lr((frak(p)))]) in V lr((frak(a)))}
  )\
                                                     & = lr(
    {lr([frak(p)]) in op("Spec") lr((S)) thin | thin frak(a) subset.eq phi^(- 1) lr((frak(p)))}
  )\
                                                     & = lr(
    {lr([frak(p)]) in op("Spec") lr((S)) thin | thin phi lr((frak(a))) subset.eq frak(p)}
  )\
                                                     & = V lr((phi lr((frak(a))))) , $ which means that the preimage of any closed subset of $op("Spec")(R)$ is closed in $op("Spec") lr((S))$. Hence $op("Spec") lr((phi))$ is continuous. \
  For functorality, let $phi : R arrow.r S$ and $psi : S arrow.r T$ be two ring homomorphisms. It is clear that $ lr((op("Spec")lr((phi)) circle.stroked.tiny op("Spec")lr((psi)))) lr((lr([frak(p)]))) = lr([phi^(- 1) circle.stroked.tiny psi^(- 1) lr((frak(p)))]) = lr([lr((psi circle.stroked.tiny phi))^(- 1) lr((frak(p)))]) = op("Spec") lr((psi circle.stroked.tiny phi)) lr((lr([frak(p)]))) . $

]
#proposition[
  Quotient Map Induces Spectrum Morphism][
  Let $R$ be a commutative ring and $frak(a)$ be an ideal of $R$. The quotient map $pi : R arrow.r R \/ frak(a)$ induces a homeomorphism between $op("Spec") lr((R \/ frak(a)))$ and $V lr((frak(a)))$ as a subspace of $op("Spec")(R)$
  $
    op("Spec") (pi) = pi^(- 1) : op("Spec")(R \/ frak(a)) & arrow.long.r^tilde.op V (frak(a)) subset.eq op("Spec")(R)\
    frak(p) \/ frak(a) & arrow.long.r.bar frak(p)
  $
  which enable us to identify $op("Spec") lr((R \/ frak(a)))$ with a closed subspace of $op("Spec")(R)$.

]
#proof[
  Since $op("Spec") lr((pi))$ maps prime ideals of $R \/ frak(a)$ to prime ideals of $R$ that contain $frak(a)$, $op("Spec") lr((pi))$ is a bijection between $op("Spec") lr((R \/ frak(a)))$ and $V lr((frak(a))) = lr(
    {lr([frak(p)]) in op("Spec")(R) thin | thin frak(a) subset.eq frak(p)}
  )$. And we can check that for any basis $D lr((f + frak(a)))$ of $op("Spec") lr((R \/ frak(a)))$, $ lr((op("Spec") lr((pi)))) lr((D lr((f + frak(a))))) & = lr(
    {lr([frak(p)]) in V lr((frak(a))) thin | thin pi lr((lr([frak(p)]))) in D lr((pi lr((f))))}
  )\
                                                  & = lr(
    {lr([frak(p)]) in V lr((frak(a))) thin | thin pi lr((f)) in.not pi lr((frak(p)))}
  )\
                                                  & = lr({lr([frak(p)]) in V lr((frak(a))) thin | thin f in.not frak(p)})\
                                                  & = D lr((f)) sect V lr((frak(a))) , $ which is open in $V lr((frak(a)))$. Here is the explanation of the equality $lr(
    {lr([frak(p)]) in V lr((frak(a))) thin | thin pi lr((f)) in.not pi lr((frak(p)))}
  ) = lr({lr([frak(p)]) in V lr((frak(a))) thin | thin f in.not frak(p)})$. $pi lr((f)) in.not pi lr((frak(p))) arrow.r.double.long f in.not frak(p)$ is clear. For the other direction, if $pi lr((f)) in pi lr((frak(p)))$, then $f = p + a$ for some $p in frak(p)$ and $a in frak(a)$. Note $frak(a) subset.eq frak(p)$, we have $f in frak(p)$. Therefore, we show that $lr((op("Spec")  lr((pi))))^(- 1)$ is continous. Hence $op("Spec")  lr((pi))$ is a homeomorphism.

]
#example[
  $op("Spec") lr((𝕜 lr([x]) \/ lr((x^2))))$][
  The only prime ideal of $𝕜 lr([x]) \/ lr((x^2))$ is $lr((overline(x)))$, where $overline(x)$ is the equivalence class of $x$ in $𝕜 lr([x]) \/ lr((x^2))$. \
  ~ \
  Note $𝕜 lr([x])$ is a PID. The nonzero prime ideals of $𝕜 lr([x])$ are of the form $lr((f))$ for some irreducible polynomial $f in 𝕜 lr([x])$. Suppose $lr((x^2)) subset.eq lr((f))$. Then we have $f divides x^2$, which means $lr((f)) = lr((x))$. Hence $V lr((lr((x^2)))) = lr({lr((x))})$ and $op("Spec")  lr((𝕜 lr([x]) \/ lr((x^2)))) = lr({lr((overline(x)))})$.

]
#example[
  $affine_(bb(C))^1 arrow.r affine_(bb(C))^1$ and $V lr((lr((y)))) arrow.r V lr((lr((x^2))))$][

  Given a ring homomorphism $ phi.alt : bb(C) lr([y]) & arrow.r bb(C) lr([x])\
  y                       & arrow.r.bar x^2 $ it induces a continuous map $ op("Spec")  lr((phi.alt)) : op("Spec")  lr((bb(C) lr([x]))) & arrow.r op("Spec")  lr((bb(C) lr([y])))\
  lr((x - a))                                       & arrow.r.bar { f lr((y)) in bb(C) lr([y]) divides f lr((x^2)) = g lr((x)) lr((x - a)) } = lr((y - a^2)) $ $op("Spec") lr((phi.alt))$ can be visualized as follows:

  #block[
  ]
  Since $ phi.alt lr((lr((y)))) & = lr({phi.alt lr((g lr((y)) y)) divides g lr((y)) in bb(C) lr([y])})\
                        & = lr(
    {g lr((phi.alt lr((y)))) phi.alt lr((y)) divides g lr((y)) in bb(C) lr([y])}
  )\
                        & = lr({g lr((x^2)) x^2 divides g lr((y)) in bb(C) lr([y])})\
                        & subset.eq lr((x^2)) , $ $phi.alt$ can induce a ring homomorphism $ psi : bb(C) lr([y]) \/ lr((y)) & arrow.r bb(C) lr([x]) \/ lr((x^2))\
  c + lr((y))                    & arrow.r.bar c + lr((x^2)) $ And the $upright(S p e c)$ functor can induce a continuous map $ op("Spec")  lr((psi)) : op("Spec") lr((bb(C) lr([x]) \/ lr((x^2)))) & arrow.r op("Spec")  lr((bb(C) lr([y]) \/ lr((y))))\
  lr((overline(x)))                                          & arrow.r.bar lr((0)) $

]
#example[
  $V lr((I)) subset.eq affine_𝕜^m arrow.r V lr((J)) subset.eq affine_𝕜^n$][ Given a ring homomorphism $ phi.alt : 𝕜 lr([y_1 , dots.h.c , y_n]) & arrow.r 𝕜 lr([x_1 , dots.h.c , x_m])\
  y_i                                       & arrow.r.bar f_i lr((x_1 , dots.h.c , x_m)) $ If $I$ is an ideal of $𝕜 lr([x_1 , dots.h.c , x_m])$ and $J$ is an ideal of $𝕜 lr([y_1 , dots.h.c , y_n])$, and $phi.alt lr((I)) subset.eq J$, then $phi.alt$ induces a continuous map $ op("Spec") lr((phi.alt)) :op("Spec")  lr((𝕜 lr([x_1 , dots.h.c , x_m]) \/ I)) & arrow.r op("Spec")  lr((𝕜 lr([y_1 , dots.h.c , y_n]) \/ J))\
  lr((overline(x_1 - a_1) , dots.h.c , overline(x_n - a_n)))               & arrow.r.bar lr(
    (overline(y_1 - f_1 lr((a_1 , dots.h.c , a_m))) , dots.h.c , overline(y_n - f_n lr((a_1 , dots.h.c , a_m))))
  ) $

]
#proposition[
  Localization Map Induces Spectrum Morphism][
  Let $R$ be a commutative ring and $S$ be a multiplicative subset of $R$. Then the localization map $l : R arrow.r S^(- 1) R$ induces a homeomorphism
  $
    op("Spec") (l) = l^(- 1) :op("Spec") (S^(- 1) R) & arrow.r^tilde.op {
      [frak(p)] in op("Spec") (R) divides frak(p) sect S = diameter
    }\
    S^(- 1) frak(p) & arrow.r.bar frak(p)
  $

]<localization_map_induces_spectrum_morphism>
#proof[
  According to communitative algebra, we know that $op("Spec")  lr((l))$ is a bijection. For any basis $D lr((f / s))$ of $op("Spec")  lr((S^(- 1) R))$, $ lr((op("Spec")  lr((l)))) lr((D lr((f / s)))) & = lr(
    {lr([frak(p)]) in op("Spec")  lr((R)) thin | thin l lr((lr([frak(p)]))) in D lr((l lr((f / s)))) , #h(0em) frak(p) sect S = diameter}
  )\
                                           & = lr(
    {lr([frak(p)]) in op("Spec")  lr((R)) thin | thin f / s in.not l lr((frak(p))) , #h(0em) frak(p) sect S = diameter}
  )\
                                           & = lr(
    {lr([frak(p)]) in op("Spec")  lr((R)) thin | thin f / s in.not S^(- 1) frak(p) , #h(0em) frak(p) sect S = diameter}
  )\
                                           & = lr(
    {lr([frak(p)]) in op("Spec")  lr((R)) thin | thin f in.not frak(p) , #h(0em) frak(p) sect S = diameter}
  )\
                                           & = lr(
    {lr([frak(p)]) in op("Spec")  lr((R)) thin | thin frak(p) sect S = diameter}
  ) sect D lr((f)) , $ which is open in $lr({frak(p) in op("Spec")  lr((R)) thin | thin frak(p) sect S = diameter})$. Here is the explanation of the equality $ lr(
    {lr([frak(p)]) in op("Spec")  lr((R)) thin | thin f / s in.not S^(- 1) frak(p) , #h(0em) frak(p) sect S = diameter}
  ) = lr(
    {lr([frak(p)]) in op("Spec")  lr((R)) thin | thin f in.not frak(p) , #h(0em) frak(p) sect S = diameter}
  ) . $ $f / s in.not S^(- 1) frak(p) arrow.r.double.long f in.not frak(p)$ is clear. For the other direction, if $f / s in S^(- 1) frak(p)$, then $u lr((f t - p s)) = 0$. Therefore, we show that $lr((op("Spec")  lr((l))))^(- 1)$ is continous. Hence $op("Spec")  lr((l))$ is a homeomorphism.

]

There are two special cases of localization. The first one is as follows.

#example[
  $op("Spec") lr((R_(frak(p))))$][
  The localization of $R$ at a prime ideal $frak(p)$ is denoted by $R_(frak(p))$. Then $l : R arrow.r R_(frak(p))$ induces a homeomorphism $ op("Spec")  lr((R_(frak(p)))) tilde.equiv lr(
    {lr([frak(q)]) in op("Spec")  lr((R)) thin | thin frak(q) subset.eq frak(p)}
  ) $

]
#proof[
  $
    op("Spec") lr((R_(frak(p)))) & tilde.equiv lr(
    {lr([frak(q)]) in op("Spec")(R) thin | thin frak(q) sect lr((R - frak(p))) = diameter}
  ) = lr(
    {lr([frak(q)]) in op("Spec")(R) thin | thin frak(q) subset.eq frak(p)}
  ) .
  $

]
#example[
  $op("Spec") (𝕜 lr([x])_(lr((x))))$][
  The prime ideals of $𝕜 lr([x])_(lr((x)))$ are $lr((0))$ and $lr((x))$.

]
#proof[
  Note $𝕜 lr([x])$ is a PID. The nonzero prime ideals of $𝕜 lr([x])$ are of the form $lr((f))$ for some irreducible polynomial $f in 𝕜 lr([x])$. Suppose $lr((f)) subset.eq lr((x))$. Then we have $x divides f$, which means $lr((f)) = lr((x))$. Hence $ lr(
    {lr([frak(p)]) in op("Spec") lr((𝕜 lr([x]))) thin | thin frak(p) subset.eq lr((x))}
  ) = lr({lr((0)) , lr((x))}) $ and $ op("Spec") lr((𝕜 lr([x])_(lr((x))))) = lr({lr((0)) , lr((x / 1))}) . $

]
The second case of localization is as follows.

#example[
  $op("Spec") lr((R_f))$][
  The localization of $R$ at $f in R$, which is denoted by $R_f$. In this case, we have
  $
    "Spec" (R_f) arrow.r^tilde.op D_f subset.eq "Spec" (R) .
  $

]
#proof[
  According to , we have $ op("Spec") lr((R_f)) & tilde.equiv lr(
    {lr([frak(q)]) in op("Spec")(R) thin | thin frak(q) sect lr((lr({f^n in R thin | thin n in bb(Z)}))) = diameter}
  )\
                   & = lr(
    {lr([frak(q)]) in op("Spec")(R) thin | thin forall n gt.eq 1 , #h(0em) f^n in.not frak(q)}
  )\
                   & = lr({lr([frak(q)]) in op("Spec")(R) thin | thin f in.not sqrt(frak(q))})\
                   & = lr({lr([frak(q)]) in op("Spec")(R) thin | thin f in.not frak(q)})\
                   & = D lr((f)) . $

]
#lemma[
  Let $R$ be a ring and $f , g in R$. Suppose $V lr((f)) subset.eq V lr((g))$, or equivalently, $D lr((g)) subset.eq D lr((f))$. Then

  #block[
    #set enum(numbering: "(i)", start: 1)
    + $f$ is invertible in $R_g$, or equivalently, $g^n = r f$ for some $n gt.eq 1$ and $r in R$.

    + there is a canonical ring map $R_f arrow.r R_g$, $a \/ f^n arrow.r.bar a \/ f^n$.

    + there is a canonical $R_f$-module map $M_f arrow.r M_g$ for any $R$-module $M$.
  ]

]<localization_canonical_maps>
#proof[

  + $f$ is invertible in $R_g$ if and only if there exists $a in R$ such that $r f = g^n$ for some $n gt.eq 1$. By , since $V lr((f)) subset.eq V lr((g))$, we have $g^n in lr((f))$ for some $n gt.eq 1$, which proves $f$ is invertible in $R_g$.

  + #block[Suppose $l_f : R arrow.r R_f$ and $l_g : R arrow.r R_g$ are the localization maps. Since from (i) we know $ l_g lr((lr({f^n thin | thin n in bb(Z)}))) subset.eq R_g^times , $ there exists a unique ring homomorphism
      $
        rho_(f , g) : R_f & --> R_g\
        a / f^n & arrow.long.bar a / f^n
      $
      such that the following diagram commutes
      #commutative_diagram($R_f edge("rr", rho_(f,g), "-->") & & R_g\
        &R edge("ul", l_f, ->, #left)edge("ur", l_g, ->, #right)&$)

    ]

]
#proposition[
  Irreducibility of $op("Spec")(R)$
][
  Let $R$ be a commutative ring.

  + the irreducible components of $op("Spec")(R)$ are in bijection with the minimal prime ideals of $R$.

  + $op("Spec")(R)$ is irreducible if and only if $A$ has only one minimal prime ideal.

  + If $R$ is an integral domain, then $op("Spec") lr((A))$ is irreducible.
]
#definition[
  Generic Point
][
  A point $p in X$ is a #strong[generic point] for a closed subset $C$ if $overline({ p }) = C$.
]
==== Quasi-compactness <quasi-compactness>
In algebraic geometry, by convention, we use the term "quasi-compactness" to refer to the compactness of a topological space.

#proposition[
  Quasicompactness of $op("Spec")(R)$
][
  Let $R$ be a commutative ring. Then $op("Spec")(R)$ is quasi-compact.
]
#proof[
  Suppose $op("Spec")(R) = union.big_(i in I) D lr((f_i))$, where $f_i in R$. Then we see $ V lr((lr({f_i})_(i in I))) = sect.big_(i in I) V lr((f_i)) = op("Spec")(R) - union.big_(i in I) D lr((f_i)) = diameter . $ By , the ideal generated by $lr({f_i})_(i in I)$ is $R$. Hence there exists $i_1 , dots.h.c , i_n in I$ such that $1 in lr((f_(i_1) , dots.h.c , f_(i_n)))$. Therefore, we have $ op("Spec")(R) = union.big_(i in I) D lr((f_i)) = union.big_(k = 1)^n D lr((f_(i_k))) . $

]
#corollary[
  Quasi-compactness of $D lr((f))$][ Let $R$ be a commutative ring and $f in R$. Then $D lr((f))$ is quasi-compact.

]
#proof[
  $D lr((f)) tilde.equiv op("Spec") lr((R_f))$.

]
#lemma[
  Let $R$ be a commutative ring and $U subset.eq op("Spec")(R)$ be an open set. The following are equivalent:

  + $U$ is retrocompact in $op("Spec")(R)$.

  + $U$ is quasi-compact.

  + $U$ is a finite union of distinguished open subsets of $op("Spec")(R)$.

  + There exists a finitely generated ideal $frak(a) subset.eq R$ such that $D lr((frak(a))) = U$.

]
#proof[
  - (i) $arrow.r.double.long$ (ii). If $U$ is retrocompact in $op("Spec")(R)$, then $i : U arrow.r.hook op("Spec")(R)$ is quasi-compact. Since $op("Spec")(R)$ is quasi-compact, $i^(- 1) lr((op("Spec")(R))) = U$ is quasi-compact.

  - (ii) $arrow.r.double.long$ (iii). If $U$ is quasi-compact, since $U$ can be written as a union of distinguished open subsets of $op("Spec")(R)$, it is a finite union of distinguished open subsets.

  - (iii) $arrow.r.double.long$ (i). Suppose $U = union.big_(i = 1)^n D lr((f_i))$, where $f_i in R$. To show $U$ is retrocompact in $op("Spec")(R)$, it suffices to show that for any compact open set $V$ of $op("Spec")(R)$, $V sect U$ is quasi-compact in $V$. Given any quasi-compact open set $V subset.eq op("Spec")(R)$, $V$ can be written as a finite union of distinguished open subsets of $op("Spec")(R)$, say $V = union.big_(j = 1)^m D lr((g_j))$. Then we have $ V sect U = lr((union.big_(i = 1)^n D lr((f_i)))) sect.big lr((union.big_(j = 1)^m D lr((g_j)))) = union.big_(i = 1)^n union.big_(j = 1)^m D lr((f_i)) sect D lr((g_j)) = union.big_(i = 1)^n union.big_(j = 1)^m D lr((f_i g_j)) . $ Since $D lr((f_i g_j))$ are quasi-compact in $V$, we see $V sect U$ is a finite union of quasi-compact sets and accordingly $V sect U$ is quasi-compact in $V$.

  - (iii) $arrow.long.l.r.double$ (iv). $U = union.big_(i = 1)^n D lr((f_i))$, where $f_i in R$ is equivalent to $U = D lr((lr((f_1 , f_2 , dots.h.c , f_n))))$.

]
=== Structure Sheaf on $op("Spec")(R)$ <structure-sheaf-on-mathopmathrmspecleftrright>

#example[
  Sheaf Associated to a Module $M$][ Let $R$ be a commutative ring and $M$ be an $R$-module. Then we can define a presheaf $tilde(M)$ on #link(<base_of_zariski_topology>)[distinguished open sets] of $op("Spec")(R)$ as follows:

  #functor_diagram(
    F: $tilde(M)$,
    C: $mathsf("BZar")_R^(op("op"))$,
    D: $R"-"mathsf("Mod")$,
    g: $iota$,
    X: $D(g)$,
    Y: $D(f)$,
    Fg: $op("Res")_(D(f)arrow.hook.l D(g))$,
    FX: $tilde(M) lr((D(g)))=M_g$,
    FY: $tilde(M) lr((D(f)))=M_f$,
    Fg_e: $rho_(f,g)$,
    FX_e: $m / f^n$,
    FY_e: $m / f^n$,
    contravariant: true,
  )
  where $rho_(f , g) : M_f arrow.r M_g$ is the canonical $R_f$-module map
  defined in @localization_canonical_maps. Note here we abuse notation because
  $tilde(M) (D (f)) := M_f$ is only defined up to canonical isomorphism.
  Suppose $[frak(p)] in op("Spec") (R)$. The stalk of $tilde(M)$ at
  $[frak(p)]$ is given by
  $
    tilde(M)_([frak(p)]) = injlim_([frak(p)] in D (f) in mathsf("BZar")_R) tilde(M) (
      D (f)
    ) = injlim_(f in R - frak(p)) M_f = M_(frak(p)) .
  $


  We can check the sheaf condition for $tilde(M)$ as follows: Suppose
  $D (f) = limits(union.big)_(i = 1)^n D (g_i)$ and
  $ D (g_i g_j) = D (g_i) sect D (g_j) = union.big_(k = 1)^(m_(i j)) D (h_k^(i j)) . $

  According to @distinguished_open_sets_inclusion_implies_generating_unit_ideal, we see
  $g_1 \/ 1 , g_2 \/ 1 , dots.h.c , g_n \/ 1$ generate $R_f$. Then we have
  the following exact sequence
  $
    0 arrow.r.long M_f arrow.long.r^alpha xor.big_(i = 1)^n (M_f)_(g_i) arrow.long.r^beta xor.big_(i , j = 1)^n (
      M_f
    )_(g_i g_j) .
  $

  Since $D (f g_i) = D (f) sect D (g_i) = D (g_i)$, the exact sequence
  becomes
  $
    0 arrow.long.r M_f arrow.long.r^alpha xor.big_(i = 1)^n M_(g_i) arrow.long.r^beta xor.big_(i , j = 1)^n M_(g_i g_j) .
  $

  Since
  $gamma^(i j) : M_(g_i g_j) arrow.r.hook limits(xor.big)_(k = 1)^(m_(i j)) M_(h_k^(i j))$
  is injective, there exists a unique map $gamma$ such that the following diagram commutes
  #commutative_diagram($
    &display(xor.big_(k = 1)^(m_(i j)) M_(h_k^(i j)))\
    M_(g_i g_j)edge("r", iota, ->) edge("ur", gamma^(i j), ->)& display(xor.big_(k = 1)^(m_(i j))M_(g_i g_j))edge("u", gamma, "-->")
  $)
  So we get the following exact sequence
  $
    0 arrow.long.r M_f arrow.long.r^alpha xor.big_(i = 1)^n M_(g_i) arrow.long.r^(gamma circle.stroked.tiny beta) xor.big_(i , j = 1)^n xor.big_(k = 1)^(m_(i j)) M_(h_k^(i j)) .
  $

  According to @extended-sheaf-has-the-same-stalks, we can extend $tilde(M)$ to a unique
  sheaf on $op("Spec") (R)$, which is still denoted by $tilde(M)$, and call it *the sheaf associated to $M$*.

]<sheaf_associated_to_module>

#definition[Structure Sheaf on $op("Spec")(R)$][

  Let $R$ be a commutative ring. The #strong[structure sheaf] $cal(O)_(op("Spec")(R))$ on distinguished open sets of $op("Spec")(R)$ is defined as follows:
  $
    cal(O)_(op("Spec")(R)) lr((D lr((f)))) = R_f .
  $
  By @sheaf_associated_to_module, we can extend $cal(O)_(op("Spec")(R))$ to a sheaf on $op("Spec")(R)$ as follows:
  $
    cal(O)_("Spec" (R)) (U) = &projlim_(D (f) subset.eq U) cal(O)_("Spec" (R)) (D (f)) = projlim_(D (
      f
    ) subset.eq U) R_f\
    =& {
      (s_f) in product_(D (f) subset.eq U) R_f mid(|) "res"_(D (f) arrow.hook.l D (g)) (
        s_f
      ) = s_g upright("for any ") D (g) subset.eq D (f) subset.eq U
    } upright(". ")
  $


  We still denote the extended sheaf by $cal(O)_(op("Spec")(R))$, and call it the #strong[structure sheaf] on $op("Spec")(R)$.

  Alternatively, we can define the structure sheaf $cal(O)_(op("Spec")(R))$ explicitly as follows:


  + #emph[Map on object set]: For any open subset $U$ of $op("Spec")(R)$, $cal(O)_(op("Spec")(R)) lr((U))$ is the set of all functions $s : U arrow.r product.co_(lr([frak(p)]) in U) R_(frak(p))$ such that

    #block[
      #set enum(numbering: "(a)", start: 1)
      + For any $lr([frak(p)]) in U$, $s lr((lr([frak(p)]))) in R_(frak(p))$.

      + For any $lr([frak(p)]) in U$, there exists a neighborhood $V$ of $lr([frak(p)])$ in $U$ and elements $a , f in R$ such that for any $lr([frak(q)]) in V$, $f in.not frak(q)$ and $s lr((lr([frak(q)]))) = a / f$ in $R_(frak(q))$.
    ]

  + #emph[Map on morphism set]: For any inclusion $U subset.eq V$ of open subsets of $op("Spec")(R)$, the restriction map $op("res")_(V arrow.hook.l U) : cal(O)_(op("Spec")(R)) lr((V)) arrow.r cal(O)_(op("Spec")(R)) lr((U))$ is defined as follows: for any $s in cal(O)_(op("Spec")(R)) lr((V))$, $op("res")_(V arrow.hook.l U) lr((s))$ is the restriction of $s$ to $U$.


]<structure_sheaf_on_spectrum>

#definition[Affine Scheme][
  An *affine scheme* is a locally ringed space
  $(op("Spec")(R) , cal(O)_(op("Spec")(R)))$ where $op("Spec") (R)$ is a spectrum of
  a commutative ring $R$ and $cal(O)_(op("Spec")(R))$ is the #link(<structure_sheaf_on_spectrum>)[structure sheaf]
  on $op("Spec") (R)$.
]<affine_scheme>

#proposition[][
  Let $R$ be a ring. Let $M$ be an $R$-module. Let
  $tilde(M)$ be the sheaf of
  $cal(O)_(upright(S p e c) (R) )$-modules associated to $M$.

  + We have $Gamma (op("Spec")(R) , tilde(M)) = M$ as an $R$-module.

  + For every $f in R$ we have $Gamma (D (f) , tilde(M)) = M_f$ as an $R_f$-module.

  + Whenever $D (g) subset.eq D (f)$ the restriction mappings on $tilde(M)$ are the maps $M_f arrow.r M_g$ from @sheaf_associated_to_module.

  + Let $[frak(p)] in op("Spec")(R)$. We have $tilde(M)_[frak(p)] = M_(frak(p))$ as an $R_(frak(p))$-module.
]

#corollary[][
  Let $R$ be a ring.
  + We have $Gamma (op("Spec")(R) , cal(O)_(op("Spec")(R))) = R$.

  + For every $f in R$ we have $Gamma (D (f) , cal(O)_(upright(S p e c) (R))) = R_f$.

  + Whenever $D (g) subset.eq D (f)$ the restriction mappings on $cal(O)_(upright(S p e c) (R))$ are the maps $R_f arrow.r R_g$ from @sheaf_associated_to_module.

  + Let $[frak(p)] in op("Spec")(R)$. We have $cal(O)_(upright(S p e c) (R) , x) = R_(frak(p))$.
]


== Scheme
#definition[Scheme][
  A *scheme* is a ringed space $(X , cal(O)_X)$ such that for any point $x in X$, there exists an open neighborhood $U$ of $x$ such that $(U , cal(O)_X|_U)$ is isomorphic to an #link(<affine_scheme>)[affine scheme] as a ringed space.
]

== Properties of Schemes
=== Integral Schemes
#definition[Integral Scheme][
  A scheme $X$ is said to be *integral* if it is nonempty and for any affine open subset $U subset.eq X$, the ring $cal(O)_X (U)$ is an integral domain.
]

=== Reduced Schemes
#definition[Reduced Scheme][
  A scheme $X$ is said to be *reduced* if for any open subset $U subset.eq X$, the ring $cal(O)_X (U)$ is reduced.
]
#proposition[
  An affine scheme $op("Spec")(R)$ is reduced if and only if $R$ is reduced.
]
#proof[
  If $R$ is reduced, then $cal(O)_(op("Spec")(R)) lr((D lr((f)))) = R_f$ is reduced for any $f in R$. Hence $op("Spec")(R)$ is reduced.
]

#proposition[Reducedness is a Stalk-local Property][
  A scheme $X$ is reduced if and only if every local ring $cal(O)_(X , x)$ is reduced.
]
#proof[
  Suppose $X$ is a reduced scheme. Choose any point $x in X$ and any germ $f in cal(O)_(X , x)$. If $f^n=0$ for some integer $n gt.eq 1$, then there exists an open neighborhood $U$ of $x$ and a representative of $f$ denoted by $(U , f|_U)$, such that $(f|_U)^n=0$. Since $cal(O)_X (U)$ is reduced, there must be $f|_U=0$, which implies $f=0$.

  Conversely, suppose $cal(O)_(X , x)$ is reduced for every point $x in X$. Choose any open subset $U subset.eq X$. From @section_is_determined_by_its_germs_at_all_points we see
  $
    iota: cal(O)_X (U) --> product_(x in U) cal(O)_(X , x)
  $
  is injective. Let
  $
    pi_x:product_(x in U) cal(O)_(X , x)-> cal(O)_(X , x)
  $
  be projections. For any $f in cal(O)_X (U)$, if $f^n=0$ for some integer $n gt.eq 1$, then $(pi_x circle.tiny iota(f))^n=0$ for every $x in U$. Since $cal(O)_(X , x)$ is reduced, we have $pi_x circle.tiny iota(f)=0$ for every $x in U$, which implies $f=0$. Hence $cal(O)_X (U)$ is reduced.
]




#pagebreak()

= Algebraic Curves

In this chapter, by curve we mean a smooth, projective, algebraic variety of dimension 1.