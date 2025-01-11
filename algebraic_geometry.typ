#import "@preview/cetz:0.3.1"
#import "@preview/fletcher:0.5.4" as fletcher: diagram, node, edge

#import "@local/math-notes:0.2.0": *

#show: math_notes.with(title: "ALGEBRAIC GEOMETRY")


#let cal(x) = math.class("unary", text(font: "Computer Modern Symbol", x))


#let tildecal(x) = if (x.text == "F") {
  $accent(cal(F)#h(0.3em), ~)#h(-0.3em)$
} else {
  $tilde(cal(#x))$
}


#let sheafify(x) = $cal(#x)^(#h(0.2em)op("sh"))$

#let spec(x) = $op("Spec")(#x)$

#let mathsf(x) = $sans(upright(#x))$

#let res(V, U) = $op("res")_(#V arrow.l.hook #U)$

#let affine = $bold(upright(A))$

#let rightarrow = $stretch(->, size: #15pt)$

#let leftarrow = $stretch(<-, size: #15pt)$

#let movebase(size, x) = text(baseline: size)[#x]

#let (varprojlim, varinjlim) = (leftarrow, rightarrow).map(arrow => $display(limits(lim_(movebase(#(-1.9pt),arrow))))$)

#let injlim(subscript) = $varinjlim_movebase(#(-2.8pt), subscript)$
#let projlim(subscript) = $varprojlim_movebase(#(-2.8pt), subscript)$

#let xrightarrow = $stretch(->, size: #150%)$


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

  Note that $lr((tau , supset.eq))$ is also a filtered set which can be seen as the filtered (0,1)-category $mathsf("Open")_X^(op("op"))$.
  Therefore, $mathsf("Open")_X$ is both filtered and cofiltered.
]

Note in $mathsf("Open")_X$, all diagrams are commutative.



#definition[
  $mathsf(C)$-valued Presheaf
][
  Let $mathsf(C)$, $mathsf(D)$ be categories. A $mathsf(C)$-valued #strong[presheaf] is a functor $F : mathsf(D)^(op("op")) arrow.r mathsf(C)$.
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
    F: $underline(A)_op("pre")$,
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

#proposition[Monomorphisms and Epimorphisms in $mathsf("PSh")_(mathsf("Set")) lr((X))$][
  Let $X$ be topological space and $f: cal(F)->cal(G)$ be a morphism in $mathsf("PSh")_(mathsf("Set")) lr((X))$. The following conditions on $f$ are equivalent characterizations of monomorphisms:

  + $f$ is a monomorphism.

  + For any open set $U subset.eq X$, the map $f_U : cal(F)(U) arrow.r cal(G)(U)$ is injective.

  The following conditions on $f$ are equivalent characterizations of epimorphisms:

  + $f$ is an epimorphism.

  + For any open set $U subset.eq X$, the map $f_U : cal(F)(U) arrow.r cal(G)(U)$ is surjective.

  The following conditions on $f$ are equivalent characterizations of isomorphisms:

  + $f$ is an isomorphism.

  + For any open set $U subset.eq X$, the map $f_U : cal(F)(U) arrow.r cal(G)(U)$ is bijective.
]


#definition[
  Pushforward Presheaf
][
  Let $X$ be a topological space and $cal(F)$ be a presheaf on $X$. Let $f : X arrow.r Y$ be a continuous map. The #strong[pushforward presheaf] $f_(*) cal(F)$ is the presheaf on $Y$ defined as follows:

  #functor_diagram(
    F: $f_* cal(F)$,
    C: $mathsf("Open")_Y^(op("op"))$,
    D: $mathsf("C")$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $U$,
    Fg: $res(f^(-1)(V),f^(-1)(U))$,
    FX: $cal(F)(f^(-1)(V))$,
    FY: $cal(F)(f^(-1)(U))$,
  )

  The #strong[pushforward presheaf functor] $f_(*) : mathsf("PSh")_(mathsf(C)) lr((X)) arrow.r mathsf("PSh")_(mathsf(C)) lr((Y))$ is defined as follows:

  #functor_diagram_square_cd(
    F: $f_*$,
    C: $mathsf("PSh")_(mathsf(C))(X)$,
    D: $mathsf("PSh")_(mathsf(C))(Y)$,
    g: $phi$,
    X: $cal(F)_1$,
    Y: $cal(F)_2$,
    Fg: $f_* phi$,
    FX: $f_* cal(F)_1$,
    FY: $f_* cal(F)_2$,

    A11: $cal(F)_1(f^(-1)(V))$,
    A12: $cal(F)_1(f^(-1)(U))$,
    A21: $cal(F)_2(f^(-1)(V))$,
    A22: $cal(F)_2(f^(-1)(U))$,
    Ff: $res(f^(-1)(V),f^(-1)(U))$,
    Gf: $res(f^(-1)(V),f^(-1)(U))$,
    theta_l: $phi_(f^(-1)(V))$,
    theta_r: $phi_(f^(-1)(U))$,
  )
]<pushforward-presheaf>
#proof[
  Let's check that $f_*$ is a functor.

  - Suppose $cal(F)$ is a presheaf on $X$. For any open set $U$ of $Y$, we have
    $
      (f_* op("id")_(cal(F)))_U = op("id")_(cal(F)(f^(-1)(U))) = op("id")_(f_* cal(F))_U,
    $
    which means $f_* op("id")_(cal(F)) = op("id")_(f_* cal(F))$.

  - Suppose $cal(F)_1 xrightarrow^phi cal(F)_2 xrightarrow^psi cal(F)_3$ are morphisms of presheaves on $X$. For any open set $U$ of $Y$, we have
    $
      (f_* psi circle.stroked.tiny f_* phi)_U =
      psi_(f^(-1)(U)) circle.stroked.tiny phi_(f^(-1)(U)) =(psi circle.stroked.tiny phi)_(f^(-1)(U))= f_* ( psi circle.stroked.tiny phi )_U,
    $
    which means $f_* psi circle.stroked.tiny f_* phi = f_* (psi circle.stroked.tiny phi)$.
]


#example[Pushforward Presheaf along Inclusion][
  Let $(X,tau)$ be a topological space and $cal(F) in op("Ob")(mathsf("PSh")_mathsf("C")(X))$. Let $i: U arrow.hook X$ be the inclusion of an open subset $U subset.eq X$ into $X$. The #strong[pushforward presheaf along the inclusion $i$] is defined as follows:
  #functor_diagram(
    F: $i_(*)cal(F)$,
    C: $mathsf("Open")_X^(op("op"))$,
    D: $mathsf("C")$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $W$,
    Fg: $res(V sect U,W sect U)$,
    FX: $cal(F)(i^(-1)(V))=cal(F)(V sect U)$,
    FY: $cal(F)(i^(-1)(U))=cal(F)(W sect U)$,
  )

]

Since the continuous image of an open set is generally not open, defining the pushforward presheaf needs to "take outer limit" , i.e. take colimit among open sets that contain the image of the open set.

#definition[Pullback Presheaf][
  Let $X$ be a topological space and $cal(G)$ be a presheaf on $Y$. Let $f : (X,tau^') arrow.r (Y,tau)$ be a continuous map. The #strong[pullback presheaf] $f^*cal(G)$ is the presheaf on $X$ defined as follows:

  #functor_diagram(
    F: $f^* cal(G)$,
    C: $mathsf("Open")_X^(op("op"))$,
    D: $mathsf("C")$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $U$,
    Fg: $$,
    FX: $injlim_(f(V) subset.eq W in tau) cal(G)(W)$,
    FY: $injlim(f(U) subset.eq W in tau) cal(G)(W)$,
  )

  Formally, Given an open set $U$ of $X$, let $mathsf("Open")_(Y , f(U))$ be the full subcategory of $mathsf("Open")_Y$ whose
  objects are the open neighborhoods of $f(U)$ and whose morphisms are the inclusions of open sets. We see $mathsf("Open")_(Y , f(U))^(op("op"))$ is a filtered category. Therefore, $f^*cal(G)(U)$ is the filtered colimit
  $
    f^*cal(G)(U) = injlim(f(U) subset.eq W in tau) cal(G)(W) = injlim cal(G)|_mathsf("Open")_(Y , f(U))^(op("op")).
  $

  And we can define the #strong[pullback presheaf functor] $f^* : mathsf("PSh")_(mathsf(C)) lr((Y)) arrow.r mathsf("PSh")_(mathsf(C)) lr((X))$ as follows:

  #functor_diagram_square_cd(
    F: $f^*$,
    C: $mathsf("PSh")_(mathsf(C))(Y)$,
    D: $mathsf("PSh")_(mathsf(C))(X)$,
    g: $phi$,
    X: $cal(G)_1$,
    Y: $cal(G)_2$,
    Fg: $f^* phi$,
    FX: $f^* cal(G)_1$,
    FY: $f^* cal(G)_2$,

    A11: $injlim(f(V) subset.eq W ) cal(G)_1(W)$,
    A12: $injlim(f(U) subset.eq W ) cal(G)_1(W)$,
    A21: $injlim(f(V) subset.eq W) cal(G)_2(W)$,
    A22: $injlim(f(U) subset.eq W) cal(G)_2(W)$,
    Ff: $res(f(V),f(U))$,
    Gf: $res(f(V),f(U))$,
    theta_l: $(f^* phi)_V$,
    theta_r: $(f^* phi)_U$,
  )
]
#remark[
  The universal property of the colimit
  $
    f^*cal(G)(U) = injlim(f(U) subset.eq W in tau) cal(G)(W) = injlim cal(G)|_mathsf("Open")_(Y , f(U))^(op("op")).
  $
  is given as follows: for any open sets $W,Z$ such that $f(U)subset.eq W subset.eq Z$, for any set $A$, and for any morphism $h_Z: cal(G)(Z) arrow.r A$ and $h_W: cal(G)(W) arrow.r A$ such that $h_W circle.stroked.tiny res(Z , W) = h_Z$, there exists a unique morphism $tilde(h):f^*cal(G)(U) arrow.r A$ such that the following diagram commutes:

  #commutative_diagram($
    &A&\
    & f^*cal(G)(U) edge("u",#left,tilde(h), "-->")&\
    cal(G)( Z )edge("ru",#right, #h(-1em)accent(op("res"),->)_(Z,f(U)),->)edge("ruu",h_Z,->) edge("rr",#right, res(Z,W),->)&& cal(G)( W )edge("lu",#left, accent(op("res"),->)_(W,f(U))#h(-1em),->)edge("luu",h_W,->)
  $)

  If $U arrow.r.hook V$ is an inclusion map of open sets, then we have inclusion of sets $f(U) arrow.r.hook f(V)$ and inclusion of categories $mathsf("Open")_(Y , f(V)) arrow.r.hook mathsf("Open")_(Y , f(U))$. This induces a morphism $f^*cal(G)(V) ->f^*cal(G)(U)$ by the universal property of the colimit

  #commutative_diagram($
    cal(G)(W)edge("r",accent(op("res"),->)_(W,f(V)),->) edge("rd",#right,accent(op("res"),->)_(W,f(U)),->) & injlim(f( V ) subset.eq W in tau) cal(G)(W)edge("d",#left,f^*cal(G)(U arrow.hook V), "-->")\
    &injlim(f(U) subset.eq W in tau) cal(G)(W)
  $)

]


#proposition[$f^* tack.l f_*$][
  Let $f : X arrow.r Y$ be a continuous map. We have the following adjunction
  #adjunction_pair(
    C: $mathsf("PSh")_(mathsf(C))(Y)$,
    D: $mathsf("PSh")_(mathsf(C))(X)$,
    L: $f^*$,
    R: $f_*$,
  )
  For any $cal(F) in op("Ob")(mathsf("PSh")_(mathsf(C))(X))$ and $cal(G) in op("Ob")(mathsf("PSh")_(mathsf(C))(Y))$, we have natural isomorphism
  $
    "Hom"_(mathsf("PSh")_(mathsf(C))( X )) lr((f^* cal(G), cal(F))) tilde.equiv "Hom"_(mathsf("PSh")_(mathsf(C)) lr((Y))) lr((cal(G) , f_* cal(F))) .
  $
]
#proof[
  Suppose $cal(F) in op("Ob")(mathsf("PSh")_(mathsf(C))(X))$ and $cal(G) in op("Ob")(mathsf("PSh")_(mathsf(C))(Y))$. We need to define a map
  $
    Phi_(cal(G), cal(F)):"Hom"_(mathsf("PSh")_(mathsf(C))( X )) lr((f^* cal(G), cal(F))) --> "Hom"_(mathsf("PSh")_(mathsf(C)) lr((Y))) lr((cal(G) , f_* cal(F))) .
  $
  Let $theta: f^* cal(G) arrow.r cal(F)$ be a morphism in $mathsf("PSh")_(mathsf(C))(X)$. We can define a morphism $Phi_(cal(G), cal(F))(theta): cal(G) arrow.r f_* cal(F)$ in $mathsf("PSh")_(mathsf(C))(Y)$ as follows: for each open set $W subset.eq Y$, the morphism $(Phi_(cal(G), cal(F))(theta))_W: cal(G)(W)->f_* cal(F)(W)$ is the composition of the following morphisms
  $
    cal(G)(W) xrightarrow^(accent(op("res"),->)_(W,f(f^(-1)(W)))) f^* cal(G)(f^(-1)(W)) xrightarrow^(theta_(f^(-1)(W))) cal(F)( f^(-1)(W) ).
  $
  To verufy the naturality of $Phi_(cal(G), cal(F))(theta)$, we can check the following commutative diagram
  #commutative_diagram(
    $
      cal(G)(W) edge("r",res(W, W'),->) edge("d",accent(op("res"),->)_(W,f(f^(-1)(W))),->) &cal(G)( W' )edge("d",accent(op("res"),->)_(W',f(f^(-1)(W'))),->)\
      f^* cal(G)(f^(-1)(W)) edge("r",res(f^(-1)(W), f^(-1)(W')),->)edge("d",theta_(f^(-1)(W)),->)&f^* cal(G)( f^(-1)(W') ) edge("d",#left, theta_(f^(-1)(W')),->)\
      cal(F)(f^(-1)(W)) edge("r",#right,res(f^(-1)(W), f^(-1)(W')),->) &cal(F)(f^(-1)(W'))\
    $,
    spacing: (7em, 3em),
  )
  #par(first-line-indent: 0em)[which follows from the naturality of $injlim$ and $theta$.]

  Let $xi: cal(G) arrow.r f_* cal(F)$ be a morphism in $mathsf("PSh")_(mathsf(C))(Y)$. We can define a morphism $Psi_(cal(G), cal(F))(xi): f^* cal(G) arrow.r cal(F)$ in $mathsf("PSh")_(mathsf(C))(X)$ as follows: for each open set $U subset.eq X$, the morphism $(Psi_(cal(G), cal(F))(xi))_U: f^* cal(G)(U)->cal(F)(U)$ is defined through the universal property

  #commutative_diagram($
    cal(G)(W)edge("r",accent(op("res"),->)_(W,f(U)),->) edge("d",#right,xi_(W),->) & injlim(f( U ) subset.eq W in tau) cal(G)(W)edge("d",#left,(Psi_(cal(G), cal(F))(xi))_U, "-->")\
    cal(F)(f^(-1)(W)) edge("r",#right, res(f^(-1)(W), U),->)&cal(F)(U)
  $)

  The naturality of $Psi_(cal(G), cal(F))(xi)$ follows from the naturality of $xi$ and $injlim$.

  Now we check that $Phi_(cal(G), cal(F))$ and $Psi_(cal(G), cal(F))$ are inverse to each other. Let $theta: f^* cal(G) arrow.r cal(F)$ be a morphism in $mathsf("PSh")_(mathsf(C))(X)$. We are going to check that $Psi_(cal(G), cal(F))(Phi_(cal(G), cal(F))(theta)) = theta$. For any open set $U subset.eq X$, and for any $W supset.eq f(U)$, we have $U subset.eq f^(-1)(W)$ and the following commutative diagram
  #commutative_diagram(
    $
      cal(G)( W ) edge("r",accent(op("res"),->)_(W,f(U)),->) edge("d",#right, accent(op("res"),->)_(W,f(f^(-1)(W))),->) & injlim(f( U ) subset.eq W in tau) cal(G)(W)edge("dd",#left, (Psi_(cal(G), cal(F))(Phi_(cal(G), cal(F))(theta)))_U, "-->")\
      injlim(f(f^(-1)(W)) subset.eq Z in tau) cal(G)( Z )edge("ru",#right,f^*cal(G)(U arrow.hook f^(-1)(W)),->) edge("d",#right,theta_(f^(-1)(W)),->)&\
      cal(F)(f^(-1)(W)) edge("r",#right, res(f^(-1)(W), U),->) &cal(F)(U)\
    $,
    spacing: (7em, 3em),
  )
  If $(Psi_(cal(G), cal(F))(Phi_(cal(G), cal(F))(theta)))_U=theta_U$, then by the naturality of $theta$, the above diagram commutes. And the uniqueness of the colimit guarantees $(Psi_(cal(G), cal(F))(Phi_(cal(G), cal(F))(theta)))_U=theta_U$. Hence we show that $Psi_(cal(G), cal(F))circle.tiny Phi_(cal(G), cal(F))=op("id")$.

  Similarly, we can show that $Phi_(cal(G), cal(F))circle.tiny Psi_(cal(G), cal(F))=op("id")$. Let $xi: cal(G) arrow.r f_* cal(F)$ be a morphism in $mathsf("PSh")_(mathsf(C))(Y)$. We are going to check that $Phi_(cal(G), cal(F))(Psi_(cal(G), cal(F))(xi)) = xi$. For any open set $W subset.eq Y$, we have the following commutative diagram

  #commutative_diagram($
    cal(G)( W )edge("r",accent(op("res"),->)_(W,f(f^(-1)(W))),->) edge("d",#right,xi_(W),->) edge("rd",#right,xi_(W),->) & injlim(f( f^(-1)(W)) subset.eq W in tau) cal(G)(W)edge("d",#left,(Psi_(cal(G), cal(F))(xi))_(f^(-1)(W)), "-->")\
    cal(F)(f^(-1)(W)) edge("r",#right, res(f^(-1)(W), f^(-1)(W)),->)&cal(F)(f^(-1)(W))
  $)

  Hence we have $(Phi_(cal(G), cal(F))(Psi_(cal(G), cal(F))(xi)))_W = xi_W$. Therefore, we have shown that $Phi_(cal(G), cal(F))$ and $Psi_(cal(G), cal(F))$ are inverse to each other.
]

#example[Pullback Presheaf along Inclusion][
  Let $(X,tau)$ be a topological space and $cal(G) in op("Ob")(mathsf("PSh")_mathsf("Set")(X))$. Let $i: U arrow.hook X$ be the inclusion of an open subset $U subset.eq X$ into $X$. The #strong[pullback presheaf along the inclusion $i$] is defined as follows: for any open set $V subset.eq U$, we have
  $
    i^* cal(G)(V) = injlim(V subset.eq W in tau) cal(G)(W) = cal(G)(V).
  $
  Therefore, we see $i^* cal(G) = cal(G)|_U$. For any $cal(F) in op("Ob")(mathsf("PSh")_mathsf("Set")(U))$ and any open set $V subset.eq U$, we have
  $
    i^* circle.tiny i_* cal(F)(V)= i^* cal(F)(V sect U) = cal(F)(V sect U) = cal(F)(V),
  $
  which implies $i^* circle.tiny i_* = op("id")_(mathsf("PSh")_mathsf("Set")(U))$.

]<pullback-presheaf-along-inclusion>


=== Stalk of a Presheaf <stalk-of-a-presheaf>
#definition[
  Stalk of a $mathsf(C)$-valued Presheaf
][
  Let $cal(F)$ be a $mathsf(C)$-valued presheaf on a topological space $lr((X , tau))$. The #strong[stalk] of $cal(F)$ at
  a point $x in X$ is defined as the colimit
  $
    cal(F)_x := injlim(x in U in tau) cal(F) lr((U)) .
  $

  where $U$ runs over all open neighborhoods of $x$. Formally, let $mathsf("Open")_(X , x)$ be the full subcategory of $mathsf("Open")_X$ whose
  objects are the open neighborhoods of $x$ and whose morphisms are the inclusions of open sets. Since for any $U , V in tau$,
  there exists $U sect V in tau$ such that $x in U sect V$, $U supset.eq U sect V$ and $V supset.eq U sect V$, we see $mathsf("Open")_(X , x)^(op("op"))$ is
  a filtered category. Therefore, $cal(F)_x$ is a filtered colimit
  $
    cal(F)_x = injlim cal(F) |_mathsf("Open")_(X , x)^(op("op")).
  $
  The universal property of the colimit is given as follows: for open sets $W subset.eq U$, for any set $A$, and for any morphism $f_U: cal(F)(U) arrow.r A$ and $f_U: cal(F)(U) arrow.r A$ such that $h_W circle.stroked.tiny res(U , W) = f_U$, there exists a unique morphism $tilde(f):cal(F)_x arrow.r A$ such that the following diagram commutes:
  #commutative_diagram($
    &A&\
    &cal(F)_x edge("u",#left,tilde(f), "-->")&\
    cal(F)( U )edge("ru",#right, op("res")_(U,x),->)edge("ruu",f_U,->) edge("rr",#right, op("res")_(U arrow.l.hook W),->)&& cal(F)( W )edge("lu",#left, op("res")_(W,x),->)edge("luu",h_W,->)
  $)
]
#definition[
  Stalk of a $mathsf(S e t)$-valued Presheaf
][
  Let $lr((X , tau))$ be a topological space. For any $mathsf(S e t)$-valued presheaf $cal(F)$ on $X$ and for any $x in X$,
  the #strong[stalk] of $cal(F)$ at a point $x in X$ always exists because in $mathsf(S e t)$ all filtered colimits
  exists. The stalk $cal(F)_x$ can described explicitly as the quotient set

  $
    cal(F)_x = injlim cal(F) |_mathsf("Open")_(X , x)^(op("op"))=( product.co_(U in tau , U in.rev x) cal(F) (U) ) \/ tilde.op = {(U , f) divides x in U in tau , f in cal(F) (U)} \/ tilde.op
  $

  where $tilde.op$ is the equivalence relation defined as follows: for any open neighborhoods $U , V$ of $x$ and any $f in cal(F) lr((U))$, $g in cal(F) lr((V))$,
  $
    lr((U , f)) tilde.op lr((V , g)) <==> upright("there exists an open neighborhood ") W subset.eq U sect V upright("of ") x upright("such that ") f lr(|""_W = g|)_W .
  $
  The image under the map $op("res")_(U,x):cal(F) lr((U)) arrow.r cal(F)_x$ of a section $f in cal(F) lr((U))$ is the equivalence
  class of $lr((U , f))$, denoted as $lr([lr((U , f))])_x$, called the #strong[germ] of $f$ at $x$.

]<stalk-of-a-set-valued-presheaf>
#proposition[][
  Let $mathsf(C)$ be a category. Let $F : mathsf(C) arrow.r mathsf(S e t)$ be a functor. Assume that

  #block[
    #set enum(numbering: "(i)", start: 1)
    + $F$ is faithful,

    + directed colimits exist in $mathsf(C)$ and $F$ commutes with them.
  ]

  Let $cal(F)$ be a $mathsf(C)$-valued presheaf on a topological space $X$ and $x in X$. Then

  $
    cal(F)_x := injlim(x in U in tau) cal(F) lr((U)) .
  $
  exists in $mathsf(C)$. Its underlying set is equal to the stalk of the underlying presheaf of sets of $cal(F)$, i.e. $F lr((cal(F)_x)) = lr((F circle.stroked.tiny cal(F)))_x$.

]

#proposition[Stalk of Pullback Presheaf][
  Let $X$ be a topological space and $cal(G)$ be a presheaf on $Y$. Let $f : (X,tau^') arrow.r (Y,tau)$ be a continuous map. Suppose $cal(G)$ is a $mathsf(C)$-valued presheaf on $Y$ and all filtered colimits exist in $mathsf(C)$. For any $x in X$, we have the following isomorphism
  $
    (f^* cal(G))_x tilde.equiv injlim(x in U in tau^') (f^* cal(G)) (U) tilde.equiv injlim(f(x) in W in tau) cal(G)( W ) = cal(G)_(f(x)).
  $
]
#proof[
  The basic idea is
  $
    (f^* cal(G))_x &tilde.equiv injlim(x in U in tau^') (f^* cal(G)) (U)\
    &tilde.equiv injlim(x in U in tau^') injlim(f(U) subset.eq W in tau) cal(G)(W)\
    &tilde.equiv injlim(f(x) subset.eq W in tau) cal(G)(W) \
    &tilde.equiv cal(G)_(f(x)).
  $
]


#definition[
  Stalk Functor
][
  The construction of $cal(F)_x$ is functorial in the presheaf $cal(F)$. In other words, we can define a #strong[stalk functor] $injlim(x in U in tau) cal(F) lr((U)) :mathsf("PSh")_(mathsf(C))(X)->mathsf(C)$ as
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
          F: $injlim(x in U in tau)$,
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
          F: $injlim(x in U in tau)$,
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

]<stalk-functor>


=== Presheaf on Topological Base
<presheaf-on-a-base-for-topology-space>
#definition[
  Category of $mathsf(C)$-valued Presheaves on a Base for Topology
][
  Let $X$ be a topological space. Let $cal(B)$ be a base for the topology on $X$. The #strong[category of $mathsf(C)$-valued presheaves on $cal(B)$] is
  defined as $ mathsf("PSh")_(mathsf(C)) lr((cal(B))) := lr([mathsf(B)^(op("op")) , mathsf(C)]) , $ where $mathsf(B)$ is the
  category whose objects are the elements of $cal(B)$ and whose morphisms are the inclusions of elements of $cal(B)$.

]
#definition[
  Stalk of a $mathsf(C)$-valued Presheaf on a Base for Topology
][
  Let $cal(F)$ be a $mathsf(C)$-valued presheaf on a topological space $X$. Let $cal(B)$ be a base for the topology on $X$.
  For any $x in X$, the #strong[stalk] of $cal(F)$ at a point $x in X$ is defined as the colimit
  $
    cal(F)_x := injlim(x in B in cal(B)) cal(F) lr((B)) .
  $
  where $B$ runs over all elements of $cal(B)$ containing $x$. Formally, let $mathsf(B)_x$ be the full subcategory of $mathsf(B)$ whose
  objects are the elements of $cal(B)$ containing $x$ and whose morphisms are the inclusions of elements of $cal(B)$.
  Since for any $B , C in cal(B)$ such that $x in B sect C$, there exists $D in cal(B)$ such that $x in D subset.eq B sect C$,
  we see $mathsf(B)_x^(op("op"))$ is a filtered category. Therefore, $cal(F)_x$ is a filtered colimit
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
    cal(F)_x = injlim cal(F) |_mathsf(B)_(x)^(op("op"))= ( product.co_(B in cal(B) , x in B) cal(F) (B) ) \/ op(tilde.op) = {(B , f) divides x in B in cal(B) , f in cal(F) (B)} \/ tilde.op
  $
  where $tilde.op$ is the equivalence relation defined as follows: for any $B , C in cal(B)$ such that $x in B sect C$ and
  any $f in cal(F) lr((B))$, $g in cal(F) lr((C))$,
  $
    lr((B , f)) tilde.op lr((C , g)) <==> "there exists an element" D in cal(B) upright("such that") x in D subset.eq B sect C "and" f lr(|""_D = g|)_D .
  $

]



== Sheaf <sheaf>
=== Sheaf on Topological Space <sheaf-on-topological-space>
Let $lr((X , tau))$ be a topological space. Given a tuple of open sets $lr((U_i))_(i in I)$ or equivalently given a map $I arrow.r tau$,
we can define a preorder $lt.eq$ on $I times I$ by $ lr((i_1 , i_2)) lt.eq lr((j_1 , j_2)) <==> i_1 = j_1 = j_2 upright("or ") i_2 = j_1 = j_2 . $ Then $lr((I times I , lt.eq))$ can
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

    + #block[for any tuple of open sets $lr((U_i))_(i in I)$, $cal(F)$ preserves the limit of $K_I^(op("op"))$, i.e., $cal(F)$ maps a colimit of $K_I$ to a limit of $cal(F) circle.stroked.tiny K_I^(op("op"))$
        $
          cal(F)lr((injlim K_I)) = projlim cal(F) circle.stroked.tiny K_I^(op("op"))
        $
        If we denote $U = union.big_(i in I) U_i$, then the limit cone of $cal(F) circle.stroked.tiny K_I^(op("op"))$ is
        #commutative_diagram($
          &cal(F)( U ) edge("ld",op("res")_(U arrow.l.hook  U_(i_1)),->)edge("rd",op("res")_(U arrow.l.hook U_(i_1) sect thick U_(i_2)),"->")& \
          cal(F)(U_(i_1))edge("rr",op("res")_(U_(i_1)arrow.l.hook U_(i_1) sect thick U_(i_2)),->, #right)&&cal(F)( U_(i_1) sect thick U_(i_2) )
        $)
      ]

    + #block[for any open set $U subset.eq X$, for any open covering $U = union.big_(i in I) U_i$, the diagram
        $
          cal(F)(U)stretch(->, size: #3em) product_(i in I ) cal(F)( U_i ) stretch(arrows.rr , size: #3em)^(alpha_1)_(alpha_2) product_((i_1 , i_2) in I times I) cal(F) ( U_(i_1) sect U_(i_2) )
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
    cal(F)(emptyset)stretch(->, size: #3em) T stretch(arrows.rr , size: #3em)^(op("id")_T)_(op("id")_T) T
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
      cal(F)(U) stretch(->, size: #3em) product_(i in I ) cal(F)( U_i )stretch(arrows.rr , size: #3em)^(j_1)_(j_2) product_((i_1 , i_2) in I times I) cal(F) (U_(i_1) sect U_(i_2))
    $
    is an equalizer diagram.
]

#proposition[Monomorphisms and Epimorphisms in $mathsf("Sh")_(mathsf("Set")) lr((X))$][
  Let $X$ be a topological space and $f: cal(F)->cal(G)$ be a morphism in $mathsf("Sh")_(mathsf("Set")) lr((X))$. The following conditions on $f$ are equivalent characterizations of monomorphisms:

  + $f$ is a monomorphism.

  + For any open set $U subset.eq X$, the map $f_U : cal(F)(U) arrow.r cal(G)(U)$ is injective.

  + For any $x in X$, the map $f_x : cal(F)_x arrow.r cal(G)_x$ is injective.

  The following conditions on $f$ are equivalent characterizations of epimorphisms:

  + $f$ is an epimorphism.

  + For any open set $U subset.eq X$ and every section $s in cal(G)(U)$, there exists an open cover $U= union.big_(i in I) U_i$ such that for each $i in I$, $s|_(U_i)$ is in the image of the map $f_(U_i) : cal(F)(U_i) arrow.r cal(G)(U_i)$.

  + For any $x in X$, the map $f_x : cal(F)_x arrow.r cal(G)_x$ is surjective.

  The following conditions on $f$ are equivalent characterizations of isomorphisms:

  + $f$ is an isomorphism.

  + For any $x in X$, the map $f_x : cal(F)_x arrow.r cal(G)_x$ is bijective.
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
    FX: ${f : V --> A divides f "is locally constant"}$,
    FY: ${f : U --> A divides f "is locally constant"}$,
  )
]<constant-sheaf>


#example[
  Pointwise Function Sheaf
][
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
    cal(F)( U ) edge(iota, "-->") edge("dr", injlim(x in U in tau), #right, "->") & limits(product)_(x in U) cal(F)_x edge("d", pi_x, #left, "->") \
    & cal(F)_x
  $)

  And we have the following commutative diagram

  #commutative_diagram($
    cal(F)(U) edge(f, "->") edge("d", iota, #right, "->") & cal(G)(U) edge("d", iota, #left, "->") \
    limits(product)_(x in U) cal(F)_x edge((f_x)_(x in U), #right, "->")& limits(product)_(x in U) cal(G)_x
  $)

]<section_is_determined_by_its_germs_at_all_points>
#proof[
  Suppose that $s , s^prime in cal(F) lr((U))$ such that $iota lr((s)) = iota lr((s^prime))$. Then for each $x in U$, $s , s^prime$ map to the same element in stalk $cal(F)_x$. This means that for every $x in U$, there exists an open $V^x subset.eq U , x in V^x$ such that $s\|_(V^x) = s^prime\|_(V^x)$. Note that $U = union.big_(x in U) V^x$ is an open covering. Thus by the uniqueness in the sheaf condition we see that $s = s^prime$.
]



#definition[
  Sheafification of a $mathsf("Set")$-valued Presheaf
][
  Let $X$ be a topological space and $cal(F)$ be a $mathsf("Set")$-valued presheaf on $X$. The #strong[sheafification of $cal(F)$] is a sheaf $sheafify(F)$ on $X$ together with a presheaf morphism $phi : cal(F) arrow.r cal(F)^(" sh")$ such that for any sheaf $cal(G)$ and any morphism $psi : cal(F) arrow.r cal(G)$, there exists a unique morphism $psi^(+) : sheafify(F) arrow.r cal(G)$ such that the following diagram commutes:

  #commutative_diagram($
    sheafify(F) edge(psi^(+), "-->") & cal(G) \
    cal(F)edge("u",phi, #left, "->") edge("ur", psi, #right, "->") &
  $)
  A construction of $phi:cal(F)->sheafify(F)$ can be given as follows: for any open set $U subset.eq X$, we define the set of #strong[compatible germs] of $cal(F)$ on $U$ as $ sheafify(F) lr((U)) := lr(size:#115%,
    {(s_x)_(x in U) in product_(x in U) cal(F)_x thin mid(|) #box(baseline: 50%, $thin &"for any" x in U , "there exists an open neighborhood" x in V subset.eq U\
            &"and a section" f in cal(F)(V) "such that for all" y in V,  s_y = op("res")_(V,y)(f)$)}
  ) . $ If $U subset.eq V subset.eq  X$, then the image of $sheafify(F) lr((V)) arrow.r.hook product_(x in V) cal(F)_x arrow.r.twohead product_(x in U) cal(F)_x$ still lies in $sheafify(F) lr((U))$, which gives a well-defined restriction map $res(V , U) : sheafify(F) lr((V)) arrow.r sheafify(F) lr((U))$. So we can define a presheaf of sets on $X$ as follows
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
  The morphism $phi: cal(F) arrow.r sheafify(F)$ is defined as
  $
    phi_U: cal(F)(U) &--> sheafify(F) (U)\
    f &arrow.bar.long (f_x:=op("res")_(U,x)(f))_(x in U)
  $ where $f_x$ is the germ of $f$ at $x$. For any sheaf $cal(G)$ and any morphism $psi : cal(F) arrow.r cal(G)$, the morphism $psi^(+) : sheafify(F) arrow.r cal(G)$ is defined as
  $
    psi^+_U: sheafify(F)(U) &--> cal(G)(U)\
    (s_x)_(x in U) &arrow.bar.long psi_U (phi_U^(-1)((s_x)_(x in U)))
  $

]<sheafification-of-a-presheaf>
#remark[
  If $U,V$ are open sets such that $U subset.eq V$, we can check the following diagram commutes
  #commutative_diagram($
    cal(F)(V) edge(phi_V, "hook->") edge("d",res(V,U), #right, "->") &sheafify(F)( V ) edge("d", res(V,U), #right, "->")edge("hook->")& limits(product)_(x in V) cal(F)_x edge("d", "->>")\
    cal(F)(U) edge(phi_U, #right, "hook->") &sheafify(F)(U) edge("hook->")& limits(product)_(x in U) cal(F)_x
  $)
  Hence the image of $sheafify(F) lr((V)) arrow.r.hook product_(x in V) cal(F)_x arrow.r.twohead product_(x in U) cal(F)_x$ lies in $sheafify(F) lr((U))$.

]

#definition[Sheafification Functor][
  Let $X$ be a topological space. The #strong[sheafification functor] $sheafify(zws) : mathsf("PSh")_(mathsf("Set")) lr((X)) arrow.r mathsf("Sh")_(mathsf("Set")) lr((X))$ is defined as follows:
  #functor_diagram(
    F: $sheafify(zws)$,
    C: $mathsf("PSh")_(mathsf("Set"))(X)$,
    D: $mathsf("Sh")_(mathsf("Set"))(X)$,
    g: $f$,
    X: $cal(F)$,
    Y: $cal(G)$,
    Fg: $f^op("sh")$,
    FX: $sheafify(F)$,
    FY: $sheafify(G)$,
  )

]



#example[
  Constant Sheaf is the Sheafification of Constant Presheaf
][
  Let $X$ be a topological space and $A$ be a set. Let $underline(A)_(op("pre"))$ be the #link(<constant-presheaf>)[constant presheaf with value $A$]. Recall that if $underline(A)_(op("pre"))$ is a sheaf, there must be $underline(A)_(op("pre"))(diameter)={*}$. Therefore, if $A$ has more than one element, $underline(A)_(op("pre"))$ is not a sheaf. We can also check that $underline(A)_(op("pre"))$ violates the gluability axiom if $A={0,1}$ and $X={0,1}$ with the discrete topology. Take sections $0 in underline(A)_(op("pre")) lr(({0}))$ and $1 in underline(A)_(op("pre")) lr(({1}))$. We cannot find a section $f in underline(A)_(op("pre")) lr((X))$ such that $f\|_({0}) = 0$ and $f\|_({1}) = 1$.

  The sheafification of $underline(A)_(op("pre"))$ is the #link(<constant-sheaf>)[constant sheaf with value $A$], denoted by $underline(A):=(underline(A)_(op("pre")))^op("sh")$. That's because the #link(<stalk-of-a-set-valued-presheaf>)[stalk] of $underline(A)_(op("pre"))$ at any point $x in X$ is
  $
    (underline(A)_(op("pre")))_x = injlim(x in U in op("Ob")(mathsf("Open")_X)) underline(A) lr((U)) = A .
  $ and the
  compatible germs of $underline(A)_(op("pre"))$ on an open set $U subset.eq X$ are just locally constant functions $f: U arrow.r A$.
]

#definition[Sheafification of a Presheaf][
  Let $X$ be a topological space. Let $(mathsf(C), F)$ be a type of
  algebraic structure. Let $cal(F)$ be a $mathsf(C)$-valued presheaf
  on $X$. Then there exists a $mathsf(C)$-valued sheaf $sheafify(F)$ and a morphism $phi:cal(F) arrow.r sheafify(F)$ in
  $mathsf("PSh")_mathsf(C)(X)$ with the following properties:

  + The map $phi:cal(F) arrow.r sheafify(F)$ identifies the underlying $mathsf("Set")$-valued sheaf of $sheafify(F)$ with the sheafification of the underlying $mathsf("Set")$-valued presheaf of $cal(F)$.

  + For any $mathsf(C)$-valued sheaf $cal(G)$ and any morphism $psi:cal(F) arrow.r cal(G)$ in $mathsf("PSh")_mathsf(C)(X)$, $cal(F) arrow.r cal(G)$ factors uniquely through $cal(F) arrow.r sheafify(F) arrow.r cal(G)$

  #commutative_diagram($
    sheafify(F) edge(psi^(+), "-->") & cal(G) \
    cal(F)edge("u",phi, #left, "->") edge("ur", psi, #right, "->") &
  $)
]


#lemma[Pushforward of a Sheaf is a Sheaf][
  Let $X$ and $Y$ be topological spaces and $cal(F)$ be a sheaf on $X$. Let $f : X arrow.r Y$ be a continuous map. According to the definition of #link(<pushforward-presheaf>)[pushforward presheaf], $f_(*) cal(F)$ is a presheaf on $Y$. We can check that $f_(*) cal(F)$ is a sheaf on $Y$.
]
#proof[
  For any open set $U subset.eq Y$, for any open covering $U = union.big_(i in I) U_i$, we see $f^(-1)(U)=union.big_(i in I) f^(-1)(U_i)$ is an open covering of $f^(-1)(U)$. Thus we have the following equalizer diagram
  $
    cal(F)(f^(-1)(U))stretch(->, size: #3em) product_(i in I ) cal(F)( f^(-1)(U_i) )stretch(arrows.rr , size: #3em)^(alpha_1)_(alpha_2) product_((i_1 , i_2) in I times I) cal(F) ( f^(-1)(U_(i_1) sect U_(i_2)) )
  $
  which means $f_(*) cal(F)$ is a sheaf on $Y$.
]
This lemma justifies the following definition.

#definition[
  Pushforward Sheaf
][
  Let $X$ be a topological space and $cal(F)$ be a $mathsf(C)$-valued sheaf on $X$. Let $f : X arrow.r Y$ be a continuous map. The #strong[pushforward sheaf] $f_(*) cal(F)$ is the pushforward presheaf $f_(*) cal(F)$ and defined explicitly as follows:

  #functor_diagram(
    F: $f_* cal(F)$,
    C: $mathsf("Open")_Y^(op("op"))$,
    D: $mathsf(C)$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $U$,
    Fg: $res(f^(-1)(V),f^(-1)(U))$,
    FX: $cal(F)(f^(-1)(V))$,
    FY: $cal(F)(f^(-1)(U))$,
  )

  The #strong[pushforward sheaf functor] $f_(*) : mathsf("Sh")_(mathsf(C)) lr((X)) arrow.r mathsf(S h)_(mathsf(C)) lr((Y))$ is defined as follows:

  #commutative_diagram({
    let F = $f_*$
    let C = $mathsf("Sh")_(mathsf(C))(X)$
    let D = $mathsf("Sh")_(mathsf(C))(Y)$
    let g = $phi$
    let X = $cal(F)$
    let Y = $cal(G)$
    let Fg = $f_* phi$
    let FX = $f_* cal(F)$
    let FY = $f_* cal(G)$
    let Fg_e = $res(V,U)$
    let FX_e = $(U,f)$
    let FY_e = $lr((U, f))$
    let g_arrow = "=>"
    let Fg_arrow = "=>"


    let width = 1.7

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
    edge(p_X, p_Y, g, g_arrow)
    edge(p_FX, p_FY, Fg, Fg_arrow, left)

    let pad = 0.3
    let mid_y = (y1 + y2) / 2
    edge(
      (pad, mid_y),
      (width - pad, mid_y),
      F,
      "->",
      decorations: cetz.decorations.wave.with(amplitude: .06, segment-length: .2, start: 10%, stop: 90%),
    )

    let A11 = $cal(F)(f^(-1)(V))$
    let A12 = $cal(F)(f^(-1)(U))$
    let A21 = $cal(G)(f^(-1)(V))$
    let A22 = $cal(G)(f^(-1)(U))$
    let Ff = $res(f^(-1)(V),f^(-1)(U))$
    let Gf = $res(f^(-1)(V),f^(-1)(U))$
    let theta_l = $phi_(f^(-1)(V))$
    let theta_r = $phi_(f^(-1)(U))$
    let Ff_arrow = "->"
    let Gf_arrow = "->"
    let theta_l_arrow = "->"
    let theta_r_arrow = "->"

    let cd_start_x = 2.8
    let cd_width = 2

    let (p_A11, p_A12, p_A21, p_A22) = (
      (cd_start_x, y1),
      (cd_start_x + cd_width, y1),
      (cd_start_x, y2),
      (cd_start_x + cd_width, y2),
    )

    node(p_A11, A11)
    node(p_A12, A12)
    node(p_A21, A21)
    node(p_A22, A22)
    edge(p_A11, p_A12, Ff, Ff_arrow)
    edge(p_A21, p_A22, Gf, Gf_arrow, right)
    edge(p_A11, p_A21, theta_l, theta_l_arrow)
    edge(p_A12, p_A22, theta_r, theta_r_arrow, left)
  })

]<pushforward-sheaf>



#example[
  Skyscraper Sheaf
][
  Suppose $X$ is a topological space with $x in X$, and $S$ is a set. Let $i_x : { x } arrow.r X$ be the inclusion. Then we can define a functor $i_(x , *) S : mathsf("Open")_X^(op("op")) arrow.r mathsf("Set")$ as follows:

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



#definition[Pullback Sheaf][
  Let $X$ and $Y$ be topological spaces and $f : X arrow.r Y$ be a continuous map. Let $(mathsf(C), F)$ be a type of algebraic structure and $cal(G)$ be a $mathsf(C)$-valued sheaf on $Y$. The #strong[pullback sheaf] $f^(-1) cal(G)$ is the sheafification of the pullback presheaf $f^* cal(G)$, defined by the formula
  $
    f^(-1) cal(G)= (f^* cal(G))^op("sh").
  $
]<pullback-sheaf>

#proposition[The Stalk of Pullback Sheaf][
  Let $X$ and $Y$ be topological spaces and $f : X arrow.r Y$ be a continuous map. Let $(mathsf(C), F)$ be a type of algebraic structure and $cal(G)$ be a $mathsf(C)$-valued sheaf on $Y$. For any $x in X$, the stalk of the pullback sheaf $f^(-1) cal(G)$ at $x$ is given by
  $
    (f^(-1) cal(G))_x = cal(G)_(f(x)).
  $
]<stalk-of-pullback-sheaf>


#proposition[$f^(-1) tack.l f_*$][
  Let $X$ and $Y$ be topological spaces and $f : X arrow.r Y$ be a continuous map. Let $(mathsf(C), F)$ be a type of algebraic structure. We have the following adjunction
  #adjunction_pair(
    C: $mathsf("Sh")_(mathsf(C))(Y)$,
    D: $mathsf("Sh")_(mathsf(C))(X)$,
    L: $f^(-1)$,
    R: $f_*$,
  )
  For any $cal(F) in op("Ob")(mathsf("Sh")_(mathsf(C))(X))$ and $cal(G) in op("Ob")(mathsf("Sh")_(mathsf(C))(Y))$, we have natural isomorphism
  $
    "Hom"_(mathsf("Sh")_(mathsf(C))( X )) lr((f^(-1) cal(G), cal(F))) tilde.equiv "Hom"_(mathsf("Sh")_(mathsf(C)) lr((Y))) lr((cal(G) , f_* cal(F))) .
  $
]<pullback-pushforward-adjunction-for-sheaves>

#example[Pullback Sheaf along Inclusion][
  Let $(X,tau)$ be a topological space and $cal(G) in op("Ob")(mathsf("Sh")_mathsf("Set")(X))$. Let $i: U arrow.hook X$ be the inclusion of an open subset $U subset.eq X$ into $X$. The #strong[pullback presheaf along the inclusion $i$] is defined as follows: for any open set $V subset.eq U$, in @pullback-presheaf-along-inclusion we have shown the pullback presheaf $i^* cal(G)$ is given by
  $
    i^* cal(G)(V) = injlim(V subset.eq W in tau) cal(G)(W) = cal(G)(V).
  $
  Since $i^* cal(F)$ is already a sheaf, we have $  i^(-1) cal(G)=i^* cal(G)=cal(G)|_U$. For any $cal(F) in op("Ob")(mathsf("Sh")_mathsf("Set")(U))$ and any open set $V subset.eq U$, we have
  $
    i^(-1) circle.tiny i_* cal(F)(V)= i^(-1) cal(F)(V sect U) = cal(F)(V sect U) = cal(F)(V),
  $
  which implies $i^(-1) circle.tiny i_* = op("id")_(mathsf("Sh")_mathsf("Set")(U))$.

  According to @stalk-of-pullback-sheaf, for any $u in U$, we have the following canonical identification of stalks
  $
    (i^(- 1) cal(F))_u=(cal(F)|_U)_u = cal(F)_u.
  $

]<pullback-sheaf-along-inclusion>

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
  Sheaf of $RR$-valued Continuous Functions][
  Let $X$ be a topological space. The #strong[sheaf of continuous functions], denoted by $C^0_X=op("Hom")_mathsf("Top")(-,RR)$, is the sheaf on $X$ defined as follows:

  #functor_diagram(
    F: $C^0_X$,
    C: $mathsf("Open")_X^(op("op"))$,
    D: $RR op("-")mathsf("CAlg")$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $U$,
    Fg: $op("res")_(V arrow.l.hook U) = iota^*$,
    FX: $C^0_X (V)$,
    FY: $C^0_X (U)$,
  )
]<sheaf-of-real-valued-continuous-functions>



#example[
  Sheaf of Continuous Sections of a Continuous Map
][
  Let $E$ and $X$ be topological spaces and $p: E arrow.r X$ be a continuous map. The #strong[sheaf of sections of $p$], denoted $Gamma (-,p)$, is the sheaf on $X$ defined as follows:

  #functor_diagram(
    F: $Gamma (-,p)$,
    C: $mathsf("Open")_X^(op("op"))$,
    D: $mathsf("Set")$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $U$,
    Fg: $op("res")_(V arrow.l.hook U)=iota^*$,
    FX: $Gamma (V,p)$,
    FY: $Gamma (U,p)$,
  )
  where
  $
    Gamma (U,p)={s : U arrow.r f^(- 1) (U) mid(|) s "is a continous map such that" f circle.stroked.tiny s = op("id")_U}
  $
  consists of local continuous sections of $p:E arrow X$ over $U subset.eq X$.

  The construction of $Gamma (-,p)$ is functorial in $p$. In other words, we can define a functor $op("SecSh") : mathsf("Top")\/X arrow.r mathsf("Sh")_mathsf("Set")(X) arrow.r.hook mathsf("PSh")_mathsf("Set")(X)$ as follows:

  #functor_diagram(
    F: $op("SecSh")$,
    C: $mathsf("Top")\/X$,
    D: $mathsf("Sh")_mathsf("Set")(X)$,
    g: $g$,
    X: $Y_1 xrightarrow^(p_1) X$,
    Y: $Y_2 xrightarrow_(p_2) X$,
    Fg: $g_*$,
    FX: $Gamma (-,p_1)$,
    FY: $Gamma (-,p_2)$,
    Fg_arrow: "=>",
    FX_e: $s$,
    FY_e: $g circle.tiny s$,
  )

]

#definition[
  Étale Space
][
  An #strong[étale map] over a topological space $X$ is an object $p : E arrow.r X$ in slice category $mathsf("Top")\/ X$ such that $p$ is a local homeomorphism. The topological space $E$ is called an #strong[étale space] over $X$. The set $E_x = p^(- 1) lr((x))$ where $x in X$ is called the #strong[stalk] of $p$ over $x$. The category of étale maps over $X$ is the full subcategory of $mathsf("Top")\/ X$ consisting of étale spaces over $X$, denoted $mathsf("Et")_X$.

]
#definition[
  Étale Space of a Presheaf
][
  Let $X$ be a topological space and $cal(F)$ be a presheaf on $X$. The #strong[étale space] of $cal(F)$, denoted $op("Et")lr((cal(F)))$, is defined as follows:

  - The underlying set of $op("Et")lr((cal(F)))$ is the disjoint union of all stalks of $cal(F)$
  $ product.co_(x in X) cal(F)_x = lr({lr((x , s)) thin | thin x in X , s in cal(F)_x}) . $

  - The topology on $op("Et")lr((cal(F)))$ is generated by the following basis:
  $
    lr(
      {lr({lr((x , s)) in product.co_(x in X) cal(F)_x mid(|) x in U}) mid(|) U upright("is open in ") X}
    ) .
  $

  Define a map $p : op("Et")lr((cal(F))) arrow.r X$ by $p lr((x , s)) = x$. Then $p$ is a local homeomorphism. Therefore, $p_cal(F) : op("Et")lr((cal(F))) arrow.r X$ is an étale map over $X$, which justifies the notation.

  This construction is functorial. In other words, we can define a functor $op("Et"): mathsf("PSh")_(mathsf("Set")) lr((X)) arrow.r mathsf("Et")_X arrow.r.hook mathsf("Top")\/ X$ as follows:


  #functor_diagram(
    F: $op("Et")$,
    C: $mathsf("PSh")_mathsf("Set")(X)$,
    D: $mathsf("Top")\/X$,
    g: $phi$,
    X: $cal(F)$,
    Y: $cal(G)$,
    Fg: $op("Et")(phi)$,
    FX: $op("Et")(cal(F)) xrightarrow^(p_cal(F)) X$,
    FY: $op("Et")(cal(G)) xrightarrow_(p_cal(G)) X$,
    g_arrow: "=>",
    FX_e: $(x,s)$,
    FY_e: $(x,phi(s))$,
  )

  We have the following adjunction
  #adjunction_pair(
    C: $mathsf("PSh")_(mathsf("Set"))(X)$,
    D: $mathsf("Top")\/ X$,
    L: $"Et"$,
    R: $op("SecSh")$,
  )
  which gives an adjoint isomorphism
  $
    "Hom"_(mathsf("Top")\/ X) lr((op("Et")lr((-)) , -)) tilde.equiv "Hom"_(mathsf("PSh")_(mathsf("Set")) lr((X))) lr((- , op("SecSh") lr((-)))) .
  $

  Furthermore, this adjunction restricts to an adjoint equivalence of categories
  #adjunction_pair(
    C: $mathsf("Sh")_(mathsf("Set"))(X)$,
    D: $mathsf("Et")_X$,
    L: $"Et"$,
    R: $op("SecSh")$,
  )

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
    cal(F)(B)stretch(->, size: #3em) product_(i in I ) cal(F)( B_i )stretch(arrows.rr , size: #3em)^(alpha_1)_(alpha_2) product_(( i , j ) in I times I) product_(k in I_(i j)) cal(F) (V_k^(i j))
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
          cal(F) (U) & := projlim((V in cal(B) , V subset.eq U)) tildecal(F) (V)\
          & = { (f_V) in product_(V in cal(B) , V subset.eq U) tildecal(F) (V) mid(|) "res"_(V arrow.l.hook W) ( f_V ) = f_W "for any" V , W in cal(B) "such that " W subset.eq V subset.eq U }
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


#definition[$f$-map between Sheaves][
  Let $f : (X,tau) arrow.r (Y,tau')$ be a continuous map and $(mathsf(C), F)$ be a type of algebraic structure. Suppose $cal(F) in op("Ob")(mathsf("Sh")_mathsf(C)(X))$ and $cal(G)in op("Ob")(mathsf("Sh")_mathsf(C)(Y))$. An $f$-map $xi : cal(G) arrow.r cal(F)$ is a collections of maps
  $
    xi=(xi_(V, U) : cal(G) (V) arrow.r cal(F) (U))_(U in tau, f(U) subset.eq V in tau')
  $
  such that for all open sets $U' subset.eq U subset.eq X$ and $V' subset.eq V subset.eq Y$ such that $f(U) subset.eq V$ and $f(U') subset.eq V'$, the following diagram commutes
  #square_cd(
    A11: $cal(G) (V)$,
    A12: $cal(G) (V')$,
    A21: $cal(F) (U)$,
    A22: $cal(F) (U')$,
    Ff: $op("res")_(V arrow.l.hook V')$,
    Gf: $op("res")_(U arrow.l.hook U')$,
    theta_l: $xi_(V, U)$,
    theta_r: $xi_(V', U')$,
  )
]




#proposition[][
  Let $f : X arrow.r Y$ be a continuous map. Let
  $cal(F)$ be a $mathsf("Set")$-valued sheaf on $X$ and let $cal(G)$ be a $mathsf("Set")$-valued sheaf on $Y$. There are
  bijections between the following 3 sets

  + $"Hom"_(mathsf("Sh")_(mathsf(C)) lr((Y))) lr((cal(G) , f_* cal(F)))$,

  + $"Hom"_(mathsf("Sh")_(mathsf(C))(
      X
    )) lr((f^(-1) cal(G), cal(F)))$,

  + The set of $f$-maps $xi : cal(G) arrow.r cal(F)$.
]<equivalent-characterization-f-map-between-ringed-spaces>
#proof[
  @pullback-pushforward-adjunction-for-sheaves.
]

#proposition[$f$-maps Induces Morphisms between Stalks][
  Let $f : X arrow.r Y$ be a continuous map. Let $(mathsf(C), F)$ be a type of algebraic structure. Suppose $cal(F) in op("Ob")(mathsf("Sh")_mathsf(C)(X))$, $cal(G)in op("Ob")(mathsf("Sh")_mathsf(C)(Y))$ and $xi: cal(G) arrow.r cal(F)$ be an $f$-map. Then we have a sheaf morphism $xi: f^(-1)cal(G) arrow.r  cal(F)$, which induces a morphism between stalks
  $
    xi_x : cal(G)_(f(x))stretch(->, size: #2em)^(tilde)(f^(-1)cal(G))_x stretch(->, size: #2em)cal(F)_(x).
  $
]<f-map-induces-morphism-between-stalks>
#proof[
  This follows from @stalk-of-pullback-sheaf and #link(<stalk-functor>)[the functorality of the stalk functor].
]

#definition[
  Ringed Spaces][
  A #strong[ringed space] is a pair $lr((X , cal(O)_X))$, where $X$ is a topological space and $cal(O)_X$ is a sheaf of commutative rings on $X$.

]

#definition[
  Category of Ringed Spaces][
  The #strong[category of ringed spaces] $mathsf("RS")$ consists of the following data:

  - #emph[Objects] : ringed spaces $lr((X , cal(O)_X))$.

  - #emph[Morphisms] : morphisms of ringed spaces $lr((f , f^(♯))) : lr((X , cal(O)_X)) arrow.r lr((Y , cal(O)_Y))$, where $f : X arrow.r Y$ is a continuous map and $f^(♯):cal(O)_Y ->cal(O)_X$ is an $f$-map. By @equivalent-characterization-f-map-between-ringed-spaces, this is equivalent to a morphism of sheaves $f^(♯)_* : cal(O)_Y arrow.r f_(*) cal(O)_X$, where $f_(*) cal(O)_X$ is the #link(<pushforward-sheaf>,"pushforward sheaf") of $cal(O)_X$ along $f$. The naturality of $f^(♯)$ is given by the following commutative diagram:
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
    For simplicity, we abuse the notation and write $f=lr((f , f^(♯)))$. Then the morphism of ringed spaces can be written as $f : lr((X , cal(O)_X)) arrow.r lr((Y , cal(O)_Y))$.
]

#proposition[Morphisms between Ringed Spaces Induce Morphisms between Stalks][
  Let $f : lr((X , cal(O)_X)) arrow.r lr((Y , cal(O)_Y))$ be a morphism of ringed spaces. Then we have a sheaf morphism $f^(♯) : f^(-1)cal(O)_Y arrow.r  cal(O)_X$, which induces a ring homomorphism between stalks
  $
    f^(♯)_x: cal(O)_(Y,f(x))--> cal(O)_(X,x).
  $
]<morphisms-between-ringed-spaces-induce-morphisms-between-stalks>
#proof[
  This is a direct consequence of @f-map-induces-morphism-between-stalks.
]

#definition[
  Locally Ringed Spaces][A ringed space $lr((X , cal(O)_X))$ is called a #strong[locally ringed space] if for every $x in X$, the stalk $cal(O)_(X , x)$ is a local ring.

]<locally-ringed-space>


#definition[
  Category of Locally Ringed Spaces
][
  The #strong[category of locally ringed spaces] $mathsf("LRS")$ consists of the following data:

  - #emph[Objects] : locally ringed spaces $lr((X , cal(O)_X))$.

  - #emph[Morphisms] : a *morphism of locally ringed spaces* $lr((f , f^(♯))) : lr((X , cal(O)_X)) arrow.r lr((Y , cal(O)_Y))$ is a morphism of ringed spaces such that for every $x in X$, the #link(<morphisms-between-ringed-spaces-induce-morphisms-between-stalks>)[induced ring homomorphism] $f_x^(♯) : cal(O)_(Y , f lr((x))) arrow.r cal(O)_(X , x)$ is a local ring homomorphism.
]
#remark[
  Denote the maximal ideal of $cal(O)_(X , x)$ by $frak(m)_(X , x)$. Recall that $f_x^(♯) : cal(O)_(Y , f lr((x))) arrow.r cal(O)_(X , x)$ is a local ring homomorphism if and only if $f_x^(♯)(frak(m)_(Y , f(x))) subset.eq frak(m)_(X , x)$. Geometrically, this means the functions vanishing at $f(x)$ must be pulled back to functions vanishing at $x$ by $f$.
]

#example[
  Continuous Real-valued Functions Structure Sheaf
][
  Let $X$ be a topological space. According to @sheaf-of-real-valued-continuous-functions, $(X, C^0_X)$ is a locally ringed space, where $C^0_X(U)$ is the sheaf of continuous real-valued functions on open set $U subset.eq X$. The stalk $C^0_(X,x)$ at $x in X$ is the ring of germs of continuous real-valued functions at $x$. The maximal ideal of $C^0_(X,x)$ is the set of germs of continuous real-valued functions vanishing at $x$, i.e.
  $
    frak(m)_(X , x) = {[g]_x in C^0_(X,x) mid(|) g(x) = 0}.
  $
  The residue field of $X$ at $x$ is the field
  $
    kappa(x) = C^0_(X,x) \/ frak(m)_(X , x) tilde.equiv RR.
  $
  If $f : X arrow.r Y$ is a continuous map, then we can define a morphism $f^♯ : C^0_Y arrow.r f_* C^0_X$ as follows: for any open set $V subset.eq Y$,
  $
    f^♯_V : C^0_Y (V) &arrow.r.long C^0_X (f^(-1)(V)),\
    h &arrow.r.long.bar h circle.tiny f|_(f^(-1)(V)).
  $
  We can check that $f^♯$ is a local ring homomorphism: for any $x in X$,
  $
    f^♯_x (frak(m)_(Y , f(x))) = { [h circle.tiny f]_(x) in C^0_(X,x) mid(|) [h]_(f(x)) in C^0_(Y,f(x)), h(f(x)) = 0 } subset.eq frak(m)_(X , x).
  $
  Therefore, $(f,f^♯) : lr((X , C^0_X)) arrow.r lr((Y , C^0_Y))$ is a morphism of locally ringed spaces.
]


#definition[
  Residue Field
][
  Let $lr((X , cal(O)_X))$ be a locally ringed space and $x in X$. The #strong[residue field] of $X$ at $x$ is the field
  $
    kappa(x)=cal(O)_(X , x) \/ frak(m)_(X , x) ,
  $
  where $frak(m)_(X , x)$ is the maximal ideal of $cal(O)_(X , x)$.

]<residue-field>

#definition[
  Open Immersions of Locally Ringed Spaces
][
  Let $f: (X, cal(O)_X) arrow.r (Y, cal(O)_Y)$ be a morphism of locally ringed spaces. We say that $f$ is an #strong[open immersion] if the following conditions are satisfied:
  - $f$ is a homeomorphism onto its image $f(X)$ where $f(X)$ is equipped with the subspace topology induced by $Y$,
  - $f^♯: f^(-1)cal(O)_Y arrow.r  cal(O)_X$ is an isomorphism of sheaves on $X$.
]<open-immersion-of-locally-ringed-spaces>



#definition[Open Subspace of Locally Ringed Spaces][
  Let $(X,cal(O)_X)$ be a locally ringed space. Let $U subset.eq X$ be an open subset of $X$. Let $cal(O)_X|_U$ be the restriction of $cal(O)_X$ to $U$. For any $u in U$, the stalk $cal(O)_(X , u)$ is isomorphic to the stalk $cal(O)_X|_(U , u)$. Therefore, the pair $(U,cal(O)_X|_U)$ is a locally ringed space, called the *open subspace of $X$ associated to $U$*. The inclusion $i: (U,cal(O)_X|_U) arrow.r (X, cal(O)_X)$ is an #link(<open-immersion-of-locally-ringed-spaces>)[open immersion] of locally ringed spaces.
]<open-subspace-of-locally-ringed-spaces>
#remark[
  The inclusion $i: (U,cal(O)_X|_U) arrow.r (X, cal(O)_X)$ is an #link(<open-immersion-of-locally-ringed-spaces>)[open immersion] of locally ringed spaces because the induced map $i^♯: i^(-1)cal(O)_X arrow.r cal(O)_X|_U$ is an isomorphism of sheaves on $U$ according to @pullback-sheaf-along-inclusion.

  Next we give an explicit discription of the isomorphism of sheaves $i^♯_*: cal(O)_X arrow.r i_(*)(cal(O)_X|_U)$. The pushforward sheaf $i_(*)(cal(O)_X|_U)$ is defined as follows:
  #functor_diagram(
    F: $i_(*)(cal(O)_X|_U)$,
    C: $mathsf("Open")_X^(op("op"))$,
    D: $mathsf("Set")$,
    g: $iota^(op("op"))$,
    X: $V$,
    Y: $W$,
    Fg: $res(V sect U,W sect U)$,
    FX: $cal(O)_X|_U (i^(-1)(V))=cal(O)_X (V sect U)$,
    FY: $cal(O)_X|_U (i^(-1)(U))=cal(O)_X (W sect U)$,
  )
  $i^♯_*: cal(O)_X arrow.r i_(*)(cal(O)_X|_U)$ is defined as follows:
  For each open set $V$ of $X$, the map
  $
    i^♯_*(V): cal(O)_X (V) arrow.r cal(O)_X (V sect U)
  $ is the restriction map $res(V sect U,V): cal(O)_X (V) arrow.r cal(O)_X (V sect U)$. And we can check the naturality of $i^♯_*$ by the following commutative diagram:

  #square_cd(
    A11: $cal(O)_Y (V)$,
    A12: $cal(O)_Y (W)$,
    A21: $cal(O)_X (V sect U)$,
    A22: $cal(O)_X (f^(-1)(U))$,
    Ff: $op("res")_(V arrow.l.hook W)$,
    Gf: $op("res")_(V arrow.l.hook W)$,
    theta_l: $(i^♯_*)_(V)$,
    theta_r: $(i^♯_*)_(U)$,
  )

]

#proposition[Forgetful Functor $mathsf("LRS") arrow.r mathsf("RS")$ Creates Isomorphisms][
  The forgetful functor $mathsf("LRS") arrow.r mathsf("RS")$ creates isomorphisms. That is, if $(X,cal(O)_X)$, $(Y,cal(O)_Y)$ are locally ringed spaces and $f: (X,cal(O)_X) arrow.r (Y,cal(O)_Y)$ is an isomorphism in $mathsf("RS")$, then $f$ is an isomorphism in $mathsf("LRS")$.
]
#proof[
  Let $f: (X,cal(O)_X) arrow.r (Y,cal(O)_Y)$ be an isomorphism in $mathsf("RS")$. We need to show that for any $x in X$, the ring isomorphism $f^♯_x: cal(O)_(Y , f(x))-> cal(O)_(X , x)$ is a local ring isomorphism. Let $frak(m)_(Y , f(x))$ and $frak(m)_(X , x)$ be the maximal ideals of $cal(O)_(Y , f(x))$ and $cal(O)_(X , x)$ respectively. Since $f^♯_x (frak(m)_(Y , f(x))) = frak(m)_(X , x)$, we see $f^♯_x$ is a local ring isomorphism. Hence $f$ is an isomorphism in $mathsf("LRS")$.
]

#proposition[][
  Let $f:(X,cal(O)_X)->(Y,cal(O)_Y)$ be a morphism of locally ringed spaces. Let $U subset.eq X$ and $V subset.eq Y$ be open subsets such that $f(U)subset.eq V$. Then there exists a unique morphism of locally ringed spaces $f|_U:U->V$ such that the following diagram is a commutative square of in $mathsf("LRS")$
  #square_cd(
    A11: $(U,cal(O)_X|_U)$,
    A12: $(V,cal(O)_Y|_V)$,
    A21: $(X,cal(O)_X)$,
    A22: $(Y,cal(O)_Y)$,
    Ff: $f|_U$,
    Gf: $f$,
    theta_l: $i$,
    theta_r: $i$,
  )
]


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

To get an intuition, we can consider a polynomial $f in bb(C) lr([x])$, and a prime ideal $frak(p) = lr((x - a))$, then $f lr((lr([frak(p)]))) = f lr((x)) + lr((x - a)) = f lr((a))$. In this case, we see $f lr((a)) = 0 <==> f in lr((x - a))$. Generally, we should make $f$ vanish by modding out by ideals, rather than through evaluation. So we have $f lr((lr([frak(p)]))) = 0 <==> f in frak(p)$, which is the same as saying $f$ vanishes at $frak(p)$. This motivates the following definition.

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
  Non-vanishing Set
][
  Given a subset $S$ of a commutative ring $R$, the #strong[non-vanishing set] of $S$ is defined as follows: $ D lr((S)) = op("Spec")(R) - V lr((S)) = lr({lr([frak(p)]) in op("Spec")(R) divides S subset.eq.not frak(p)}) . $ In particular, if $S = { f }$, then we write $D lr((f))$ instead of $D lr(({ f }))$ and call it the #strong[non-vanishing set of $f$] $ D lr((f)) = op("Spec")(R) - V lr((f)) = { lr([frak(p)]) in op("Spec")(R) divides f in.not frak(p) } . $ $D lr((dot.op))$ can be seen as a map from the power set of $R$ to the power set of $op("Spec")(R)$, that is $ D : 2^R & arrow.r 2^(thin op("Spec")(R))\
  S       & arrow.r.bar { lr([frak(p)]) in op("Spec")(R) divides S subset.eq.not frak(p) } . $

]
=== Topology on $op("Spec")(R)$<topology-on-mathopmathrmspecleftrright>
Next we define a topology on $op("Spec")(R)$, which is called Zariski topology.
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
    + If $S_1 subset.eq S_2 subset.eq R$, then we have
      $
        lr([frak(p)]) in V lr((S_2)) & arrow.r.double.long S_2 subset.eq frak(p) arrow.r.double.long S_1 subset.eq frak(p) arrow.r.double.long lr([frak(p)]) in V lr((S_1)) ,
      $ which means $V lr((S_2)) subset.eq V lr((S_1))$.

    + Since $sqrt(lr((S)))$ is the intersection of all prime ideals containing $S$, we have $ lr([frak(p)]) in V lr((S)) & <==> S subset.eq frak(p) <==> sqrt(lr((S))) subset.eq frak(p) <==> lr([frak(p)]) in V lr((sqrt(lr((S))))) , $ which means $V lr((S)) = V lr((sqrt(lr((S)))))$. Note that $sqrt(lr((f^n))) = sqrt(lr((f))^n) = sqrt(lr((f)))$, we have $V lr((f)) = V lr((f^n))$.

    + $
        V lr((0)) = lr({lr([frak(p)]) in op("Spec")(R) divides 0 in frak(p)}) = op("Spec")(R) .
      $ Since $lr((1)) = R$, we have $ V lr((1)) = V lr((R)) = lr({lr([frak(p)]) in op("Spec")(R) divides R subset.eq frak(p)}) = diameter . $

    + If $frak(a)$ is a ideal in $R$, and $V lr((frak(a))) = diameter$, then $frak(a)$ is not contained in prime ideals. Note maximal ideals are prime ideals. So $frak(a)$ is not contained in maximal ideals, which means $frak(a) = R$.

    + $
        lr([frak(p)]) in V lr((frak(a) sect frak(b))) & <==> frak(a) sect frak(b) subset.eq frak(p)\
        lr([frak(p)]) in V lr((frak(a) frak(b))) & <==> frak(a) frak(b) subset.eq frak(p)\
        & <==> frak(a) subset.eq frak(p) upright("or ") frak(b) subset.eq frak(p)\
        & <==> lr([frak(p)]) in V lr((frak(a))) upright("or ") lr([frak(p)]) in V lr((frak(b)))\
        & <==> lr([frak(p)]) in V lr((frak(a))) union V lr((frak(b))) .
      $

    + $
        lr([frak(p)]) in V lr((sum_(i in I) frak(a)_i)) & <==> sum_(i in I) frak(a)_i subset.eq frak(p)\
        & <==> frak(a)_i subset.eq frak(p) upright("for all ") i in I\
        & <==> lr([frak(p)]) in V lr((frak(a)_i)) upright("for all ") i in I\
        & <==> lr([frak(p)]) in sect.big_(i in I) V lr((frak(a)_i)) .
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
]<properties-of-function-I>

#theorem[
  Hilbert's Nullstellensatz
][
  Let $upright(R a d) lr((R))$ be the collection of all radical ideals of $R$ and $ mono(C l o s e d)_(op("Spec")(R)) := lr({A subset.eq op("Spec")(R) thin | thin A upright("is closed")}) $ be the collection of closed subsets of $op("Spec")(R)$. Then by restricting $V : 2^R arrow.r 2^(thin op("Spec")(R))$ to $upright(R a d) lr((R))$, we obtain the following bijection:
  $
    V : op("Rad")(R) & arrow.r.long^tilde.op mono("Closed")_(op("Spec") (R))\
    frak(a) & arrow.r.bar.long V (frak(a)) .
  $
  The inverse map of $V\|_(upright(R a d) lr((R)))$ is $I\|_(mono(C l o s e d)_(op("Spec")(R)))$. Furthermore, $V$ is an order isomorphism between the partial order sets $lr((upright(R a d) lr((R)) , subset.eq))$ and $lr((mono(C l o s e d)_(op("Spec")(R)) , supset.eq))$.
]

#corollary[
  Algebra-Geometry Dictionary
][
  $V: 2^R arrow.r 2^(thin op("Spec")(R))$ and $I:2^(thin op("Spec")(R))->2^R$ are mutually inverse bijections when restricting to
  $
    V : op("Spec")(R) & arrow.long.r^tilde.op mono("Irreducible_Closed_Subset")_(op("Spec")(R))\
    frak(p) & arrow.long.r.bar V (frak(p)) = overline({ frak(p) })
  $
  or
  $
    V : op("MinPrime") (R) &arrow.long.r^tilde.op mono("Irreducible_Component")_(op("Spec") (R))\
    frak(q)& arrow.long.r.bar V (frak(q)) = overline({ frak(q) })\
  $ or
  $
    V : op("MaxSpec") (R)& arrow.long.r^tilde.op mono("Closed_Singleton")_(op("Spec") (R))\
    frak(m) & arrow.long.r.bar V (frak(m)) = {frak(m)}\
  $

]<algebra-geometry-dictionary>
#proposition[
  Base of Zariski Topology][
  The collection of all sets of the form $D lr((f))$, where $f in R$, $ lr({D lr((f)) in 2^(thin op("Spec")(R)) thin | thin f in R}) $ forms a base for the Zariski topology on $op("Spec")(R)$. For this reason, we call the sets $D lr((f))$ #strong[distinguished open set]. They form a full subcategory of the category of $mathsf("Open")_(op("Spec")(R))$. We denote this category as $mathsf(B Z a r)_R$.

]<base_of_zariski_topology>
#proof[
  For any open set $D lr((S))$ of $op("Spec")(R)$, we have $ D lr((S)) & = op("Spec")(R) - V lr((S))\
            & = op("Spec")(R) - sect.big_(f in S) V lr((f))\
            & = union.big_(f in S) lr((op("Spec")(R) - V lr((f))))\
            & = union.big_(f in S) D lr((f)) . $

]
#proposition[
  Suppose $R$ is a commutative ring and $frak(a)$ be an ideal of $R$. Then $f in R$ vanishes on $V lr((frak(a))) <==> V lr((frak(a))) subset.eq V lr((f)) <==> f^n in frak(a)$ for some $n gt.eq 1$.
]<vanish_on_v>
#proof[
  $
    f in R upright("vanishes on ") V lr((frak(a))) & <==> forall lr([frak(p)]) in V lr((frak(a))) , f in frak(p)\
    & <==> f in sect.big_(lr([frak(p)]) in V lr((frak(a)))) frak(p)\
    & <==> f in sect.big_(lr([frak(p)]) in op("Spec")(R)\
    frak(a) subset.eq frak(p)) frak(p) = sqrt(frak(a))\
    & <==> f^n in frak(a) upright("for some ") n gt.eq 1\
    & <==> lr((f)) subset.eq sqrt(frak(a))\
    & <==> V lr((frak(a))) subset.eq V lr((f)) .
  $

]
#corollary[
  If $g_1 , dots.h.c , g_m in R$, then $D lr((f)) subset.eq union.big_(i = 1)^m D lr((g_i))$ if and only if $g_1 \/ 1 , dots.h.c , g_m \/ 1$ generate $R_f$ in $R_f$.

]<distinguished_open_sets_inclusion_implies_generating_unit_ideal>
#proof[
  Denote $frak(a) = lr((g_1 , dots.h.c , g_n))$. Then by we have $ D lr((f)) subset.eq union.big_(i = 1)^n D lr((g_i)) & <==> D lr((f)) subset.eq D lr((frak(a))) <==> V lr((frak(a))) subset.eq V lr((f))\
                                                      & <==> f^n in frak(a) upright("for some ") n gt.eq 1 <==> f^n = sum_(i = 1)^m a_i g_i upright("for ") a_i in R\
                                                      & <==> 1 = sum_(i = 1)^m a_i / f^n g_i / 1 <==> lr((g_1 / 1 , g_2 / 1 , dots.h.c , g_m / 1)) = R_f . $

]
#proposition[
  Functor $op("Spec"): mathsf("CRing")^(op("op"))-> mathsf("Top")$
][

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
  First, $phi^(- 1)$ is a well-defined map from $op("Spec") lr((S))$ to $op("Spec")(R)$ since $phi^(- 1)$ maps any prime ideal of $S$ to a prime ideal of $R$. Then we show that $phi^(- 1)$ is continuous. Let $V lr((frak(a)))$ be a closed subset of $op("Spec")(R)$. Then we can check that $ lr((spec(phi)))^(- 1) lr((V lr((frak(a))))) & = lr(
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

  #block[ ]
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
  $V lr((I)) subset.eq affine_𝕜^m arrow.r V lr((J)) subset.eq affine_𝕜^n$][
  Given a ring homomorphism $ phi.alt : 𝕜 lr([y_1 , dots.h.c , y_n]) & arrow.r 𝕜 lr([x_1 , dots.h.c , x_m])\
  y_i                                       & arrow.r.bar f_i lr((x_1 , dots.h.c , x_m)) $ If $I$ is an ideal of $𝕜 lr([x_1 , dots.h.c , x_m])$ and $J$ is an ideal of $𝕜 lr([y_1 , dots.h.c , y_n])$, and $phi.alt lr((I)) subset.eq J$, then $phi.alt$ induces a continuous map $ op("Spec") lr((phi.alt)) :op("Spec")  lr((𝕜 lr([x_1 , dots.h.c , x_m]) \/ I)) & arrow.r op("Spec")  lr((𝕜 lr([y_1 , dots.h.c , y_n]) \/ J))\
  lr((overline(x_1 - a_1) , dots.h.c , overline(x_n - a_n)))               & arrow.r.bar lr(
    (overline(y_1 - f_1 lr((a_1 , dots.h.c , a_m))) , dots.h.c , overline(y_n - f_n lr((a_1 , dots.h.c , a_m))))
  ) $

]
#proposition[
  Localization Map Induces Spectrum Morphism][
  Let $R$ be a commutative ring and $S$ be a multiplicative subset of $R$. Then the localization map $l : R arrow.r S^(- 1) R$ induces a homeomorphism
  $
    op("Spec") (l) = l^(- 1) :op("Spec") (S^(- 1) R) & -->^tilde.op { [frak(p)] in op("Spec") (R) divides frak(p) sect S = diameter }\
    S^(- 1) frak(p) & arrow.r.bar.long frak(p)
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
  The localization of $R$ at $f in R$, which is den oted by $R_f$. In this case, we have
  $
    spec(R_f) -->^tilde.op D_f subset.eq spec(R) .
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
  Let $R$ be a commutative ring and $f , g in R$. Suppose $V lr((f)) subset.eq V lr((g))$, or equivalently, $D lr((g)) subset.eq D lr((f))$. Then

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
]<irreducibility_of_affine_scheme>
#definition[
  Generic Point
][
  A point $p in X$ is a #strong[generic point] for a closed subset $C$ if $overline({ p }) = C$.
]

=== Topological Properties of $op("Spec")(R)$ <topological_properties_of_affine_scheme>
==== Connectedness<connectedness>

#proposition[$op("Spec")(R)$ is Not Connected $<==>$ $R=R_1 times R_2$][
  Let $R$ be a commutative ring. $op("Spec")(R)$ is not connected if and only if $R$ is isomorphic to the product of nonzero rings $R_1$ and $R_2$.
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
  Sheaf Associated to a Module $M$][
  Let $R$ be a commutative ring and $M$ be an $R$-module. Then we can define a presheaf $tilde(M)$ on #link(<base_of_zariski_topology>)[distinguished open sets] of $op("Spec")(R)$ as follows:

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
    tilde(M)_([frak(p)]) = injlim([frak(p)] in D (f) in mathsf("BZar")_R) tilde(M) ( D (f) ) = injlim(f in R - frak(p)) M_f = M_(frak(p)) .
  $


  We can check the sheaf condition for $tilde(M)$ as follows: Suppose
  $D (f) = limits(union.big)_(i = 1)^n D (g_i)$ and
  $ D (g_i g_j) = D (g_i) sect D (g_j) = union.big_(k = 1)^(m_(i j)) D (h_k^(i j)) . $

  According to @distinguished_open_sets_inclusion_implies_generating_unit_ideal, we see
  $g_1 \/ 1 , g_2 \/ 1 , dots.h.c , g_n \/ 1$ generate $R_f$. Then we have
  the following exact sequence
  $
    0 stretch(->, size: #2em) M_f stretch(->, size: #2em)^alpha xor.big_(i = 1)^n (M_f)_(g_i) stretch(->, size: #2em)^beta xor.big_(i , j = 1)^n ( M_f )_(g_i g_j) .
  $

  Since $D (f g_i) = D (f) sect D (g_i) = D (g_i)$, the exact sequence
  becomes
  $
    0 stretch(->, size: #2em) M_f stretch(->, size: #2em)^alpha xor.big_(i = 1)^n M_(g_i) stretch(->, size: #2em)^beta xor.big_(i , j = 1)^n M_(g_i g_j) .
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
    0 stretch(->, size: #2em) M_f stretch(->, size: #2em)^alpha xor.big_(i = 1)^n M_(g_i) stretch(->, size: #2em)^(gamma circle.stroked.tiny beta) xor.big_(i , j = 1)^n xor.big_(k = 1)^(m_(i j)) M_(h_k^(i j)) .
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
    cal(O)_(spec(R)) (U) = &projlim(D (f) subset.eq U) cal(O)_(spec(R)) (D (f)) = projlim(D (f) subset.eq U) R_f\
    =& { (s_f) in product_(D (f) subset.eq U) R_f mid(|) "res"_(D (f) arrow.hook.l D (g)) ( s_f ) = s_g upright("for any ") D (g) subset.eq D (f) subset.eq U } upright(". ")
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

  + Let $[frak(p)] in op("Spec")(R)$. We have $cal(O)_(upright(S p e c) (R) , [frak(p)]) = R_(frak(p))$.
]


== Scheme
=== Definition of Scheme
#definition[Scheme][
  A *scheme* is a ringed space $(X , cal(O)_X)$ such that for any point $x in X$, there exists an open neighborhood $U$ of $x$ such that $(U , cal(O)_X|_U)$ is isomorphic to an #link(<affine_scheme>)[affine scheme] as a ringed space.
]

#definition[Affine Open Subsets][
  An open subset $U subset.eq X$ of a scheme $X$ is said to be *affine* if $(U , cal(O)_X|_U)$ is an affine scheme.
]

#proposition[][
  Let $X$ be a scheme. Let $iota:U->X$ be an #link(<open-immersion-of-locally-ringed-spaces>)[open immersion of locally ringed spaces]. Then $U$ is a scheme.
]

#definition[Open Subscheme][
  Let $X$ be a scheme. If $U$ is an #link(<open-subspace-of-locally-ringed-spaces>)[open subspace of $X$], then $U$ is a scheme, and is called an *open subscheme* of $X$.
]

=== Zariski Topology of Schemes
#lemma[][
  Let $X$ be a topological space and $eta in X$. If $overline({eta})=X$ and $U$ is an nonempty open subset of $X$, then $eta in U$.
]<generic_point_in_open_set>
#proof[
  Suppose $eta in.not U$. Then $X-U$ is a closed subset of $X$ and $eta in X-U$. Hence we have $X=overline({eta}) subset.eq X-U$ in $X$, which contradicts the fact that $U$ is nonempty.
]


#proposition[Schemes are Sober Spaces][
  Suppose $X$ is a scheme. Then $X$ is a sober space. That is, every irreducible closed subset of $X$ has a unique generic point.
]<schemes_are_sober_spaces>
#proof[
  Suppose $Z subset.eq X$ is an irreducible closed subset. Given an affine open $U=op("Spec")(R) subset.eq X$ that satisfies $Z sect U eq.not emptyset$. We know nonempty open subsets of irreducible spaces are irreducible and dense. Since $Z sect U$ is an open subset of the irreducible space $Z$, we see $Z sect U$ is a irreducible dense subset of $Z$. Hence $Z sect U$ is an irreducible closed subset of $U$. By @algebra-geometry-dictionary, it corresponds to a prime ideal $frak(p)=I(Z sect U)$ of $R$ and we have $Z sect U=op("cl")_U ({frak(p)})=op("cl")_(Z sect U) ({frak(p)})$. Since $Z=op("cl")_(Z)(Z sect U)$, by transitivity of denseness, we have $overline({frak(p)})=Z$ in $X$, which means $frak(p)$ is a generic point of $Z$.

  If there exists $xi in X$ such that $overline({xi})=Z$ in $X$. By @generic_point_in_open_set, we see $xi in U$ and $xi$ can be identified with $frak(q) in op("Spec")(R)$. Then we have $V(frak(p))=V(frak(q))=overline({frak(p)})=overline({frak(q)})=Z sect U$ in $U$. By @algebra-geometry-dictionary, we have $frak(p)=frak(q)=I(Z sect U)$. Hence $Z$ has a unique generic point.
]

#proposition[Schemes are Locally Quasi-compact][
  The underlying topological space of any scheme is locally quasi-compact.
]

#definition[Dimension of a Scheme][
  Let $X$ be a scheme. The *dimension* of $X$ is the supremum of the lengths of chains of irreducible closed subsets of $X$, denote by
  $
    dim(X) = sup {n | Z_0 subset.neq Z_1 subset.neq dots.h.c subset.neq Z_n subset.eq X, quad Z_i "is irreducible closed"}
  $

]

=== Base Change of Schemes

#definition[Category of Schemes over $S$][
  Let $S$ be a scheme. The category of schemes over $S$ is the slice category $mathsf("Sch")_(S) = (mathsf("Sch")\/ S)$,

  - A *scheme over $S$* is an object $X$ in $mathsf("Sch")_(S)$, namely a
    scheme $X$ together with a morphism $g : X arrow.r S$.

  - A *morphism of schemes over $S$* is a morphism in $mathsf("Sch")_(S)$, namely a morphism $f : X arrow.r Y$ in $mathsf("Sch")$ such that the following diagram commutes
    #commutative_diagram($
      X edge("rd",#right, g, ->) edge("rr", f, ->)& & Y edge("ld",#left, h, ->) \
      & S
    $)

  Let $R$ be a commutative ring. The *category of schemes over $R$* is the category of schemes over $op("Spec")(R)$.
]

#definition[Base Change of Schemes][
  Let $S$ be a scheme and $X$ be a scheme over $S$. Given a morphism $g : S' arrow.r S$, the *base change of $X$ along $g$* is the scheme $X_(S') := S' times_S X$ over $S'$. The pullback diagram is as follows
  #square_cd(
    A11: $X_(S')$,
    A12: $X$,
    A21: $S'$,
    A22: $S$,
  )
  Let $R$ be a commutative ring and $X$ be a scheme over $R$. Given a ring homomorphism $g: R arrow.r R'$, the *base change of $X$ along $g$* is the scheme $X_(R') := op("Spec")(R') times_(op("Spec")(R)) X$ over $R'$. The pullback diagram is as follows
  #square_cd(
    A11: $X_(R')$,
    A12: $X$,
    A21: $op("Spec")(R')$,
    A22: $op("Spec")(R)$,
  )
]

#definition[Scheme Theoretic Fiber][
  Let $g : X arrow.r S$ be a morphism of schemes. The *scheme theoretic fiber* of $g$ over a point $s in S$ is the scheme $X_s :=X_(kappa(s))= op("Spec")(kappa(s)) times_S X$ over $kappa(s)$. The pullback diagram is as follows
  #square_cd(
    A11: $op("Spec")(kappa(s)) times_S X$,
    A12: $X$,
    A21: $op("Spec")(kappa(s))$,
    A22: $S$,
  )
]

#lemma[][
  Consider the following two pullback diagrams:

  #square_cd(
    A11: $op("Spec")(cal(O)_(S,s)) times_S X$,
    A12: $X$,
    A21: $op("Spec")(cal(O)_(S,s))$,
    A22: $S$,
  )
  #square_cd(
    A11: $op("Spec")(kappa(s)) times_S X$,
    A12: $X$,
    A21: $op("Spec")(kappa(s))$,
    A22: $S$,
  )

  In both cases the top horizontal arrow is a topological embedding, i.e., homeomorphism onto its image.

]



== Properties of Schemes

We say $P$ is a *property of a class of mathematical objects* if for any object $X$ in the class, $P(X)$ is a proposition that is either true or false. We say $X$ has property $P$ if $P(X)$ is true.

#definition[Stalk-local Property][
  We say a property $P$ of schemes is a *stalk-local property* if the following condition holds: for any scheme $X$, $P$ holds for $X$ if and only if $P$ holds for the spectrum of every stalk $cal(O)_(X , x)$.
]<stalk-local-property>

#proposition[Equivalent Characterizations for Stalk-local Properties][
  Let $P$ be a stalk-local property of schemes and $X$ be a scheme. The following are equivalent:

  + $X$ has property $P$.

  + The spectrum of every stalk of $X$ has property $P$.

  + Every open subscheme of $X$ has property $P$.

  + Every affine open subscheme of $X$ has property $P$.

  + $X$ has an open cover by affine open subschemes that have property $P$.

  + $X$ has an open cover by open subschemes that have property $P$.
]<equivalent_characterizations_for_stalk-local_properties>
#proof[
  By definition we have (i) $<==>$ (ii). And it is straightforward to check (iii) $==>$ (iv) $==>$ (v) $==>$ (vi).

  (ii) $==>$ (iii). Suppose every stalk of $X$ has property $P$. Let $U$ be any open subscheme of $X$. For any $x in U$, we have $cal(O)_(X , x) = cal(O)_(U , x)$, which means each stalk of $U$ has property $P$. Since $P$ is a stalk-local property, $U$ has property $P$.

  (vi) $==>$ (ii). Suppose $X$ has an open cover by open subschemes $X =union.big_(i in I) U_i$ where each $U_i$ has property $P$. Given any $x in X$, there exists some $i in I$ such that $x in U_i$. Since $P$ is a stalk-local property, $spec(cal(O)_(U_i , x))$ has property $P$, which implies $spec(cal(O)_(X , x))$ also has property $P$. By the arbitrariness of $x$, we see each stalk of $X$ has property $P$.
]

#definition[Local Property of Commutative Rings][
  We say a property $P$ of commutative rings is a *local property* if the following conditions holds:

  + For any commutative ring $R$ and any $f in R$, we have
    $
      P(R) ==> P(R_f) .
    $

  + For any commutative ring $R$ and any $f_i in R$ such that $(f_1,f_2,dots,f_n)=R$, we have
    $
      (forall i , P(R_(f_i))) ==> P(R) .
    $

  Similarly, we say a property $P$ of affine schemes is a *local property* if the following conditions hold:

  + For any affine scheme $X=op("Spec")(R)$ and any $f in R$, we have
    $
      P(X) ==> P(D(f)) .
    $

  + For any affine scheme $X=op("Spec")(R)$ and any $f_i in R$ such that $union.big_(i in I) D(f_i)=X$, we have
    $
      (forall i , P(D(f_i))) ==> P(X).
    $
]


#definition[Affine-local Property][
  Let $P$ be a property of communitative rings and $X$ be a scheme. We say $X$ is *locally $P$* if for any $x in X$, there exists an affine open neighborhood $U$ of $x$ such that $cal(O)_X (U)$ has property $P$.

  We say $Q$ is an *affine-local property* of schemes if there exists a local property $P$ of commutative rings such that for any scheme $X$,
  $
    Q(X) <==> X "is locally" P.
  $
]



#proposition[Equivalent Characterizations for Affine-local Properties][
  Let $X$ be a scheme and $P$ be a local property of commutative rings. The following are equivalent:

  + $X$ is locally $P$.

  + For any affine open $U subset.eq X$, $cal(O)_X (U)$ has property $P$.

  + There exists an affine open cover $X = union.big_(i in I) U_i$ such that each $cal(O)_X (U_i)$ has property $P$.

  + There exists an open cover $X = union.big_(j in J) X_j$ such that each open subscheme $X_j$ is locally $P$.
]
#proof[
  By definition we have (i) $<==>$ (iii). And it is straightforward to check (ii) $==>$ (iii) $==>$ (iv).

  (iv) $==>$ (i). Suppose there exists an open cover $X = union.big_(j in J) X_j$ where each open subscheme $X_j$ is locally $P$. Given any $x in X$, there exists some $j in J$ such that $x in X_j$. Since $X_j$ is locally $P$, there exists an affine open neighborhood $U subset.eq X_j$ of $x$ such that $cal(O)_(X_j) (U)=cal(O)_(X) (U)$ has property $P$. Since $U = X_j sect V$ for some open set $V subset.eq X$, $U$ is an affine open neighborhood of $x$ in $X$. Hence $X$ is locally $P$.

  (iii) $==>$ (ii). Suppose there exists an affine open cover $X = union.big_(i in I) U_i$ where each $cal(O)_X (U_i)$ has property $P$. Given any affine open $U=spec(R) subset.eq X$,
]
#proposition[Stalk-local $==>$ Affine-local][
  Let $P$ be a stalk-local property of schemes. Then $P$ is an affine-local property.
]
#proof[
  Suppose $P$ is a stalk-local property of schemes. We can define a property $tilde(P)$ of commutative rings as follows: for any commutative ring $R$, $tilde(P)(R) <==> P(op("Spec")(R))$. By @equivalent_characterizations_for_stalk-local_properties, we see $tilde(P)$ is a local property of commutative rings.
  To show $P$ is an affine-local property, it suffices to show that given any scheme $X$, $P(X) <==> X "is locally" tilde(P)$.

  If $P(X)$ holds, by (iv) of @equivalent_characterizations_for_stalk-local_properties we see for any $x in X$, there exists an affine open neighborhood $U$ of $x$ such that $P(U)$ holds, which implies $tilde(P)(cal(O)_X (U))$ holds. Hence $X "is locally" tilde(P)$.

  If $X "is locally" tilde(P)$, then for any $x in X$, there exists an affine open neighborhood $U$ of $x$ such that $tilde(P)(cal(O)_X (U))$ holds, which implies $P(U)$ holds. Since $X$ has an open cover by affine open subsets that have property $P$, by (v) of @equivalent_characterizations_for_stalk-local_properties we see $P(X)$ holds.
]

=== Quasi-compact Schemes
#definition[Quasi-compact Scheme][
  A scheme $X$ is said to be *quasicompact* if it is quasi-compact as a topological space.
]

#proposition[Equivalent Definitions of Quasi-compact Scheme][
  Let $X$ be a scheme. The following are equivalent:

  + The scheme $X$ is quasi-compact.

  + $X$ can be written as a finite union of affine open subsets.

  + The morphism $X arrow.r op("Spec") (R)$ is quasi-compact for some commutative ring $R$.
]

#proof[
  (i) $==>$ (ii). By definition of a scheme, $X$ has an open cover $X = union.big_(i in I) U_i$ such that each $U_i$ is an affine open subset of $X$. Since $X$ is quasi-compact, there exists a finite subcovering $X = union.big_(i in J) U_i$
  where $J subset.eq I$ is a finite set. Hence $X$ can be written as a finite union of affine open subsets.

  (ii) $==>$ (i). Suppose $X$ can be written as a finite union of affine open subsets $X = union.big_(i = 1)^n U_i$. Since finite unions of quasi-compact subset of $X$ is quasi-compact and each $U_i$ is quasi-compact, we see $X$ is quasi-compact.

  (iii) $==>$ (i). Suppose $f:X arrow.r op("Spec") (R)$ is quasi-compact. Since preimages of quasi-compact sets are quasi-compact, we see $X = f^(- 1) lr((op("Spec") (R)))$ is quasi-compact.

  (i) $==>$ (iii). Suppose $X$ is quasi-compact. According to @equivalent_definitions_of_quasicompact_morphism, since $op("Spec") (R)$ is an affine open cover of itself, and $f^(- 1) lr((op("Spec") (R))) = X$ is quasi-compact, we see $f$ is quasi-compact.
]

#proposition[Quasi-compact Schemes Have Closed Points][
  Let $X$ be a quasi-compact scheme. Then every point has a closed point in its
  closure. every nonempty closed subset of $X$ contains a closed point of
  $X$. Especially, $X$ has a closed point.
]

=== Noetherian Schemes

#definition[Locally Noetherian Scheme][
  A scheme $X$ is said to be *locally Noetherian* if every point $x in X$ has an affine open neighborhood $U subset.eq X$ such that the ring $cal(O)_X (U)$ is Noetherian.
]

#proposition[Equivalent Definitions of Locally Noetherian Scheme][
  Let $X$ be a scheme. The following are equivalent:

  + The scheme $X$ is locally Noetherian.

  + For every affine open $U subset X$ the ring $cal(O)_X (U)$ is Noetherian.

  + There exists an affine open covering $X = union.big U_i$ such that each $cal(O)_X (U_i)$ is Noetherian.
  + There exists an open covering $X = union.big X_j$ such that each open subscheme $X_j$ is locally Noetherian.
]

If $X$ is locally Noetherian then every open subscheme is locally Noetherian.

#definition[Noetherian Scheme][
  A scheme $X$ is said to be *Noetherian* if it is locally Noetherian and quasi-compact.
]
Being Noetherian is an affine-local property.

=== Irreducible Schemes


#proposition[Equivalent Definitions of Irreducible Scheme][
  Let $X$ be a scheme. The following are equivalent.

  + The scheme $X$ is irreducible.

  + There exists an affine open covering $X = union.big_(i in I)U_i$ such that $I$ is not empty, $U_i$ is irreducible for all $i in I$, and
    $
      U_i sect U_j eq.not emptyset
    $
    for all $i, j in I$.

  + The scheme $X$ is nonempty and every nonempty affine open $U subset.eq X$ is irreducible.
]

#corollary[][
  Let $X$ be an irreducible scheme. Then there exists a unique generic point $eta in X$ such that $overline({eta})=X$.
]
#proof[
  This is a direct consequence of @schemes_are_sober_spaces.
]

=== Reduced Schemes
#definition[Reduced Scheme][
  A scheme $X$ is said to be *reduced* if for any open subset $U subset.eq X$, the ring $cal(O)_X (U)$ is reduced.
]


#proposition[Reducedness is a Stalk-local Property][
  A scheme $X$ is reduced if and only if every local ring $cal(O)_(X , x)$ is reduced.
]<reducedness_is_a_stalk_local_property>
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

#proposition[
  An affine scheme $op("Spec")(R)$ is reduced if and only if $R$ is reduced.
]
#proof[
  Suppose $R$ is reduced. Since any localization of a reduced ring is reduced, $cal(O)_(op("Spec")(R),[frak(p)]) = R_frak(p)$ is reduced for any $frak(p) in op("Spec")(R)$. According to @reducedness_is_a_stalk_local_property, we see $op("Spec")(R)$ is reduced.

  Conversely, suppose $op("Spec")(R)$ is reduced. Then $cal(O)_(op("Spec")(R))(op("Spec")(R)) = R$ is reduced.
]

#proposition[
  Equivalent Definitions of Reduced Scheme
][
  Let $X$ be a scheme. The following are equivalent.

  + The scheme $X$ is reduced.

  + For every affine open $U subset X$ the ring $cal(O)_X (U)$ is reduced.

  + There exists an affine open covering $X = union.big U_i$ such that each $cal(O)_X (U_i)$ is reduced.

  + Every stalk $cal(O)_(X , x)$ is reduced.
]
#proof[
  Since reducedness is a stalk-local property, this proposition follows from
  @equivalent_characterizations_for_stalk-local_properties.
]


=== Integral Schemes
#definition[Integral Scheme][
  A scheme $X$ is said to be *integral* if it is nonempty and for any nonempty affine open subset $U subset.eq X$, the ring $cal(O)_X (U)$ is an integral domain.
]

#proposition[Equivlent Definitions of Integral Scheme][
  Let $X$ be a scheme. The following are equivalent.

  + $X$ is integral.

  + $X$ is reduced and irreducible.
]
#proof[
  (ii) $==>$ (i). Suppose $X$ is reduced and irreducible. Given any affine open subset $U =op("Spec")(R)subset.eq X$, $X$ is irreducible implies that $U$ is irreducible. Since $op("Spec")(R)=V((0))$ is irreducible closed in $op("Spec")(R)$, the corresponding ideal $I(op("Spec")(R))=(0)$ is a prime ideal of $R$. Since $X$ is reduced, $cal(O)_X (U) = R$ is reduced. Thus $R$ is a reduced ring with a minimal prime ideal $(0)$, which implies $R$ is an integral domain.

  (i) $==>$ (ii). Suppose $X$ is integral. Then for every affine open subset $U=op("Spec")(R)subset.eq X$, $R$ is an integral domain, which implies $R$ is reduced and $U$ is irreducible. Hence $X$ is reduced and irreducible.
]

#definition[Function Field of a Integral Scheme][
  Let $X$ be an integral scheme and $eta in X$ is the generic point of $X$. The *function field* of $X$ is defined as the stalk $cal(O)_(X , eta)$.
]

#proposition[Function Field is a Fraction Field][
  Let $X$ be an integral scheme. Then the function field of $X$ is a fraction field of the ring $cal(O)_X (U)$ for any nonempty affine open subset $U subset.eq X$.
]
#proof[
  Suppose $eta$ is the generic point of $X$. Given any nonempty affine open subset $U=op("Spec")(R) subset.eq X$, by @generic_point_in_open_set there must be $eta in U$. According to @properties-of-function-I, we have
  $
    overline({(0)})=V(I({(0)}))=V((0))=op("Spec")(R).
  $
  By the uniqueness of generic point, we see $eta$ can be identified with $(0)in op("Spec")(R)$. Since $R$ is an integral domain, we have $cal(O)_(X , eta)=cal(O)_X|_(U,eta)=R_eta=op("Frac")(R)$. Hence the function field of $X$ is the fraction field of the ring $cal(O)_X (U)$.
]

=== Normal Schemes
#definition[Normal Scheme][
  A scheme $X$ is said to be *normal* if and only if for all $x in X$, the local ring $cal(O)_(X , x)$ is a normal domain.
]

#proposition[Equivalent Definitions of Normal Scheme][
  Let $X$ be a scheme. The following are equivalent.

  + The scheme $X$ is normal.

  + For every affine open $U subset.eq X$ the ring $cal(O)_X (U)$ is normal.

  + There exists an affine open cover $X = union.big_(i in I) U_i$ such that each $cal(O)_X (U_i)$ is normal.

  + There exists an open cover $X = union.big_(i in I) U_i$ such that each open subscheme $U_i$ is normal.
]


#proposition[Normal Schemes are Reduced][
  Every normal scheme is reduced.
]

#proposition[Equivalent Definitions of Normal Integral Scheme][
  Let $X$ be an integral scheme. Then $X$ is normal if and only if for every nonempty affine open $U subset.eq X$ the ring $cal(O)_X (U)$ is a normal domain.
]

=== Regular Schemes
#definition[Regular Scheme][
  A scheme $X$ is said to be *regular* if for any point $x in X$, there exists an affine open neighborhood $U subset.eq X$ of $x$ such that the ring $cal(O)_X (U)$ is a Noetherian regular ring.
]

#proposition[Equivalent Definitions of Regular Scheme][
  Let $X$ be a scheme. The following are equivalent.

  + The scheme $X$ is regular.

  + For every affine open $U subset.eq X$ the ring $cal(O)_X (U)$ is a Noetherian regular ring.

  + There exists an affine open cover $X = union.big_(i in I) U_i$ such for each $i in I$, $cal(O)_X (U_i)$ is a Noetherian regular ring.

  + There exists an open cover $X = union.big_(i in I) U_i$ such for each $i in I$, the open subscheme $U_i$ is regular.

  + $X$ is locally Noetherian and for any point $x in X$, the stalk $cal(O)_(X , x)$ is a regular ring.

  + $X$ is locally Noetherian and for any closed point $x in X$, the stalk $cal(O)_(X , x)$ is a regular ring.
]

#proposition[Regular Schemes are Normal][
  Every regular scheme is normal.
]

=== Dedekind Schemes

#definition[Dedekind Scheme][
  A scheme $X$ is said to be *Dedekind* if it is a regular Noetherian integral scheme of dimension 1.
]

#proposition[Points of a Dedekind Scheme][
  Let $X$ be a Dedekind scheme. Then the points of $X$ consisting of the following two classes:

  + The unique generic point $eta$ of $X$, which is not closed.

  + The closed points of $X$.

  Both classes are nonempty.
]

#pagebreak()

= Morphisms of Schemes

The category of schemes $mathsf("Sch")$ is a full subcategory of the category of locally ringed spaces $mathsf("LRS")$. A morphism of schemes is a morphism of locally ringed spaces.


== Properties of Morphisms of Schemes



=== Quasi-compact Morphisms
#definition[Quasi-compact Morphism][
  A morphism $f : X arrow.r Y$ of schemes is said to be *quasi-compact* if the underlying map of topological spaces is quasi-compact, that is, for any quasi-compact open subset $V subset.eq Y$, the preimage $f^(- 1) (V)$ is quasi-compact.
]

#proposition[Equivalent Definitions of Quasi-compact Morphism][
  Let $f : X arrow.r Y$ be a morphism of schemes. The following are equivalent.

  + The morphism $f$ is quasi-compact.

  + For any affine open subset $V subset.eq Y$, the preimage $f^(- 1) (V)$ is quasi-compact.

  + There exists an affine open covering $Y = union.big_(i in I) V_i$ such that $f^(- 1) (V_i)$ is quasi-compact for each $i in I$.
]<equivalent_definitions_of_quasicompact_morphism>


#pagebreak()

= Algebraic Curves

In this chapter, by curve we mean a smooth, projective, algebraic variety of dimension 1.


