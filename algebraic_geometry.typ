#import "@preview/cetz:0.2.2"
#import "@preview/fletcher:0.5.0" as fletcher: diagram, node, edge
#import "@preview/xarrow:0.3.1": xarrow


#import "@local/math-notes:0.1.0": math_notes, definition, proposition, lemma, theorem, corollary, example, proof

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

#let mathsf(x) = $sans(upright(#x))$

// define commutative diagram
#let commutative_diagram(math_content, ..args) = align(center)[
  #v(1em, weak: true)
  #diagram(label-size: 0.8em, math_content, ..args)#v(1em, weak: true)
]


#let functor_diagram_info(F: $$, C: $$, D: $$, g: $$, X: $$, Y: $$, Fg: $$, FX: $$, FY: $$, Fg_e: $$, FX_e: $$, FY_e: $$) = {
  let width = 1.7
  let width_in = 2.3
  let width_e = 2.8
  let (y1, y2) = (0.5, 1.9)
   
  let (p_C, p_D) = ((0, 0), (width, 0))
  let (p_X, p_Y) = ((0, y1), (0, y2))
  let (p_FX, p_FY) = ((width, y1), (width, y2))
   
  node(p_C, C)
  node(p_D, D)
  node(p_X, X)
  node(p_FX, FX)
  node(p_Y, Y)
  node(p_FY, FY)
  edge(p_X, p_Y, g, "->")
  edge(p_FX, p_FY, Fg, "->", left)
   
  if (FX_e != $$ or FY_e != $$) {
    let (p_FX_e, p_FY_e) = ((width_e, y1), (width_e, y2))
    node((width_in, y1), $in.rev$)
    node((width_in, y2), $in.rev$)
    node(p_FX_e, FX_e)
    node(p_FY_e, FY_e)
    edge(p_FX_e, p_FY_e, Fg_e, "|->", left)
  }
   
  let pad = 0.3
  let mid_y = (y1 + y2) / 2
  edge(
    (pad, mid_y),
    (width - pad, mid_y),
    F,
    "->",
    decorations: cetz.decorations.wave.with(amplitude: .06, segment-length: .2, start: 10%, stop: 90%),
  )
}


#let functor_diagram(F: $$, C: $$, D: $$, g: $$, X: $$, Y: $$, Fg: $$, FX: $$, FY: $$, Fg_e: $$, FX_e: $$, FY_e: $$) = commutative_diagram(
  functor_diagram_info(F: F, C: C, D: D, g: g, X: X, Y: Y, Fg: Fg, FX: FX, FY: FY, Fg_e: Fg_e, FX_e: FX_e, FY_e: FY_e),
)

#let square_cd(A11: $$, A12: $$, A21: $$, A22: $$, Ff: $$, Gf: $$, theta_1: $$, theta_2: $$) = commutative_diagram({
  let width = 1
  let height = 1
   
  let (p_A11, p_A12, p_A21, p_A22) = ((0, 0), (width, 0), (0, height), (width, height))
   
  node(p_A11, A11)
  node(p_A12, A12)
  node(p_A21, A21)
  node(p_A22, A22)
  edge(p_A11, p_A12, Ff, "->")
  edge(p_A21, p_A22, Gf, "->", right)
  edge(p_A11, p_A21, theta_1, "->")
  edge(p_A12, p_A22, theta_2, "->", left)
})


#let injlim = $limits(limits(lim)_(xarrow(#v(-50em), width: #1.8em)))$
#let projlim = $limits(limits(lim)_(xarrow(sym:arrow.l.long, #v(-50em), width: #1.8em)))$

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
      Therefore, for any open sets $U , V$ of $X$ such that $U subset.eq V$, there is a map $upright(r e s)_(V , U) : cal(F) lr((V)) arrow.r cal(F) lr((U))$,
      called the #strong[restriction map], satisfying the following conditions:
       
      + $upright(r e s)_(U , U) = upright(i d)_(cal(F) lr((U)))$.
       
      + $upright(r e s)_(W , U) = upright(r e s)_(V , U) circle.stroked.tiny upright(r e s)_(W , V)$ whenever $U subset.eq V subset.eq W$.
    ]
   
  - Morphisms: A #strong[morphism of presheaves of sets on $X$] is a natural transformation $phi : cal(F) arrow.r cal(G)$.
    The naturality of $phi$ means that for any open sets $U , V$ of $X$ such that $U subset.eq V$, the following diagram
    commutes
    #square_cd(
      A11: $cal(F)(V)$,
      A12: $cal(F)(U)$,
      A21: $cal(G)(V)$,
      A22: $cal(G)(U)$,
      Ff: $op("res")_(V,U)$,
      Gf: $op("res")_(V,U)$,
      theta_1: $phi_(V)$,
      theta_2: $phi_(U)$,
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
]

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
    cal(F)_x = injlim cal(F) |_mathsf("Open")_(X , x)^(op("op"))=(product.co_(U in tau , U in.rev x) cal(F) (U)) \/ tilde.op = { (U , f) divides x in U in tau , f in cal(F) (U) } \/ tilde.op
  $
   
  where $tilde.op$ is the equivalence relation defined as follows: for any open neighborhoods $U , V$ of $x$ and any $f in cal(F) lr((U))$, $g in cal(F) lr((V))$, 
  $
    lr((U , f)) tilde.op lr((V , g)) arrow.l.r.double upright("there exists an open neighborhood ") W subset U sect V upright(" of ") x upright(" such that ") f lr(|""_W = g|)_W .
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
   
  #align(center, grid(
    columns: (auto, auto),
    rows: (auto,),
    align: center + horizon,
    inset: (left: 1em, right: 1em, top: 0.5em, bottom: 0.6em),
    grid.cell(functor_diagram(
      F: $injlim_(x in U in tau)$,
      C: $mathsf("PSh")_(mathsf(C))(X)$,
      D: $mathsf(C)$,
      g: $phi$,
      X: $cal(F)$,
      Y: $cal(G)$,
      Fg: $$,
      FX: $cal(F)_x$,
      FY: $cal(G)_x$,
    ), inset: (right: 4em)),
    grid.vline(),
    grid.cell(functor_diagram(
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
    ), inset: (left: 4em)),
  ))
   
]


=== Presheaf on a Base for Topology Space <presheaf-on-a-base-for-topology-space>
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
    cal(F)_x = injlim cal(F) |_mathsf(B)_(x)^(op("op"))= (product.co_(B in cal(B) , x in B) cal(F) (B)) \/ op(tilde.op) = { (B , f) divides x in B in cal(B) , f in cal(F) (B) } \/ tilde.op
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
we can define a preorder $lt.eq$ on $I times I$ by $ lr((i_1 , i_2)) lt.eq lr((j_1 , j_2)) arrow.l.r.double i_1 = j_1 = j_2 upright(" or ") i_2 = j_1 = j_2 . $ Then $lr((I times I , lt.eq))$ can
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
    Let $lr((X , tau))$ be a topological space and $mathsf(C)$ be a complete category. The #strong[category of $mathsf(C)$-valued sheaves on $X$], denoted as $sans(S h)_(mathsf(C)) lr((X))$, is a full subcategory of $sans(P S h)_(mathsf(C)) lr((X))$ defined as follows:

  - Objects: $mathsf(C)$-valued presheaves $cal(F)$ on $X$ such that one of the following equivalent condition holds:

    + #block[for any tuple of open sets $lr((U_i))_(i in I)$, $cal(F)$ preserves the limit of $K_I^(upright(o p))$, i.e., $cal(F)$ maps a colimit of $K_I$ to a limit of $cal(F) circle.stroked.tiny K_I^(upright(o p))$
    $
    cal(F)lr((injlim K_I)) = projlim cal(F) circle.stroked.tiny K_I^(upright(o p))
    $
      If we denote $U = union.big_(i in I) U_i$, then the limit cone of $cal(F) circle.stroked.tiny K_I^(upright(o p))$ is 
      #commutative_diagram($
      &cal(F)(U) edge("ld",op("res")_(U arrow.l.hook  U_(i_1)),->)edge("rd",op("res")_(U arrow.l.hook U_(i_1) sect thick U_(i_2)),"->")& \
      cal(F)(U_(i_1))edge("rr",op("res")_(U_(i_1)arrow.l.hook U_(i_1) sect thick U_(i_2)),->, #right)&&cal(F)(U_(i_1) sect thick  U_(i_2))
      $)
    ]
      
    + #block[for any open set $U subset.eq X$, for any open covering $U = union.big_(i in I) U_i$, the diagram 
    $ 
    cal(F)(U)xarrow(width: #3em, "") product_(i in I ) cal(F)(U_i)xarrow(width: #3em, sym: arrows.rr, alpha_1)_(alpha_2)product_((i_1 , i_2) in I times I) cal(F) (U_(i_1) sect U_(i_2)) 
    $ 
    is an equalizer diagram in the category $mathsf(C)$. Here $alpha_1$ and $alpha_2$ are the morphisms induced by the unversal property of product as follows: 


    where $iota_1 : U_(i_1) sect U_(i_2) arrow.r.hook U_(i_1)$ and $iota_2 : U_(i_1) sect U_(i_2) arrow.r.hook U_(i_2)$ are the inclusion maps. When $mathsf(C) = mathsf("Set")$, $alpha_1$ and $alpha_2$ can be explicitly described as above.]
    
  - Morphisms: A morphism of $mathsf(C)$-valued sheaves is a morphism of $mathsf(C)$-valued presheaves.

]


#proposition[
  Suppose $cal(F)$ is a $sans(C)$-valued presheaf on a topological space $X$.

  #block[
    #set enum(numbering: "(i)", start: 1)
    + $cal(F) lr((diameter))$ is terminal in $sans(C)$.
  ]

]


#proof[
  If we take $I = diameter$, the empty union $union.big_(i in diameter) U_i$ is the initial object $diameter$ in $sans(S e t)$, and the empty product $product_(i in diameter) cal(F) lr((U_i))$ is the terminal object $T$ in $sans(C)$. Therefore, the equalizer diagram above becomes
  $ 
    cal(F)(emptyset)xarrow(width: #3em, "") T xarrow(width: #3em, sym: arrows.rr, op("id")_T)_( op("id")_T)T
    $ 
  which means $cal(F) lr((diameter))$ is a terminal object in $sans(C)$.

]

#definition[
  Sheaf of Sets on a Topological Space
][
  Let $X$ be a topological space and $cal(F)$ is a presheaf of sets on $X$. $cal(F)$ is a #strong[sheaf of sets on $X$] if one of the following equivalent condition holds:

  + #strong[Identity axiom]. If $U = union.big_(i in I) U_i$ is an open cover of an open set $U$, and $f , g in cal(F) lr((U))$ satisfy $ upright(r e s)_(U , U_i) lr((f)) = upright(r e s)_(U , U_i) lr((g)) upright(" for all ") i in I , $ then $f = g$.

  #strong[Gluability axiom]. If $U = union.big_(i in I) U_i$ is an open cover of an open set $U$, and $lr((f_i))_(i in I) in product_(i in I) cal(F) lr((U_i))$ is a family of sections satisfying that $ upright(r e s)_(U_i , U_i sect U_j) lr((f_i)) = upright(r e s)_(U_j , U_i sect U_j) lr((f_j)) upright(" for all ") i , j in I , $ then there exists $f in cal(F) lr((U))$ such that $upright(r e s)_(U , U_i) lr((f)) = f_i$ for all $i in I$.

  + For any open set $U subset.eq X$, any open cover $U = union.big_(i in I) U_i$ and any family of sections $lr((f_i))_(i in I) in product_(i in I) cal(F) lr((U_i))$ such that $ f_i\|_(U_i sect U_j) = f_j\|_(U_i sect U_j) upright(" for all ") i , j in I , $ then there exists a unique section $f in cal(F) lr((U))$ such that $f_i = f\|_(U_i)$ for all $i in I$.

  + #block[For any open set $U subset.eq X$, and any open covering $U = union.big_(i in I) U_i$, the diagram 
  $ 
  cal(F)(U)xarrow(width: #3em, "") product_(i in I ) cal(F)(U_i)xarrow(width: #3em, sym: arrows.rr, j_1)_(j_2)product_((i_1 , i_2) in I times I) cal(F) (U_(i_1) sect U_(i_2)) 
  $ 
  is an equalizer diagram.]
]
#proposition[
  Suppose the category $sans(C)$ and the functor $U : sans(C) arrow.r sans(S e t)$ have the following properties:

    + $U$ is faithful,

    + $sans(C)$ has limits and $U$ commutes with them,

    + the functor $U$ reflects isomorphisms.


  Let $X$ be a topological space and $cal(F)$ be a $sans(C)$-valued presheaf on $X$. Then $cal(F)$ is a $sans(C)$-valued sheaf if and only if the underlying $sans(S e t)$-valued presheaf $U circle.stroked.tiny cal(F)$ is a sheaf.

]
#corollary[
  Take $sans(C) = sans(G r p) , sans(R i n g) , R upright("-") sans(M o d) , R upright("-") sans(A l g) , 𝕜 upright("-") sans(V e c t)$ and $U$ to be the forgetful functor. Let $X$ be a topological space. Let $X$ be a topological space and $cal(F)$ be a $sans(C)$-valued presheaf on $X$. Then $cal(F)$ is a $sans(C)$-valued sheaf if and only if the underlying $sans(S e t)$-valued presheaf $U circle.stroked.tiny cal(F)$ is a sheaf.

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
    Fg: $op("res")_(V,U)=iota^*$,
    FX: ${ f : V arrow.r A divides f "is locally constant" }$,
    FY: ${ f : U arrow.r A divides f "is locally constant" }$,
  )
]

#example[
  Pointwise Function Sheaf][
    Let $X$ be a topological space. Let $lr((A_x))_(x in X)$ be a family of sets $A_x$ indexed by points $x in X$. We can construct a $sans(S e t)$-valued sheaf $Pi$ as follows 
    
    #functor_diagram(
  F: $underline(A)$,
  C: $mathsf("Open")_X^(op("op"))$,
  D: $mathsf("Set")$,
  g: $iota^(op("op"))$,
  X: $V$,
  Y: $U$,
  Fg: $op("res")_(V,U)$,
  FX: $limits(product)_(x in V) A_x$,
  FY: $limits(product)_(x in U) A_x$,
  FX_e: $(a_x)_(x in V)$, 
  FY_e: $(a_x)_(x in U)$
)
where $upright(r e s)_(V , U)$ is induced by the universal property of product. Now we check this is a sheaf.

  - Identity axiom: Suppose $U = union.big_(i in I) U_i$ is an open covering of an open set $U subset.eq X$. Let $s , s^prime in Pi lr((U))$ be two sections on $U$ such that $s\|_(U_i) = s^prime\|_(U_i)$ for all $i in I$. Assume $s = lr((a_x))_(x in U) , s^prime = lr((a_x^prime))_(x in U)$. Then we have $ forall i in I , #h(0em) forall x in U_i , #h(0em) a_x = a_x^prime arrow.r.double.long forall x in U , #h(0em) a_x = a_x^prime arrow.r.double.long s = s^prime . $

  - Gluability axiom: Suppose $U = union.big_(i in I) U_i$ is an open covering of an open set $U subset.eq X$. Let $lr((s_i))_(i in I) = lr((lr((a_(i , x)))_(x in U_i)))_(i in I) in product_(i in I) Pi lr((U_i))$ be a family of sections such that $s_i$ and $s_j$ agree over $U_i sect U_j$, i.e. $ a_(i , x) = a_(j , x) upright(" in ") A_x upright(" for all ") x in U_i sect U_j . $ Let $s = lr((a_x))_(x in X)$ where $a_x = a_(i , x)$ whenever $x in U_i$ for some $i$. Then $s\|_(U_i) = s_i$ for all $i$.

]
#proposition[
  Section of a Sheaf Is Determined by Its Germs at All Points
][
  Let $X$ be a topological space and $cal(F)$ be a $sans(S e t)$-valued sheaf on $X$. For any open set $U subset.eq X$, the map $eta$ induced by the universal property of product is injective. 
]
#proof[
  Suppose that $s , s^prime in cal(F) lr((U))$ such that $eta lr((s)) = eta lr((s^prime))$. Then for each $x in U$, $s , s^prime$ map to the same element in stalk $cal(F)_x$. This means that for every $x in U$, there exists an open $V^x subset U , x in V^x$ such that $s\|_(V^x) = s^prime\|_(V^x)$. Note that $U = union.big_(x in U) V^x$ is an open covering. Thus by the uniqueness in the sheaf condition we see that $s = s^prime$.

]


= Algebraic Curves

In this chapter, by curve we mean a smooth, projective, algebraic variety of dimension.