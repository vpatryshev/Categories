## Categories Project

### Intro

This project is currently migrating from old repositories and build and testing tools.
Does not even compile yet.
Historically, it was first written in Basic, then in Fortran, then in Assembler, then in Java, now in Scala.
Long story.

The idea is to be able to do all categorical calculations, then be able to calculate topos logic and topologies.

### Current Status
Java code works, and it's frozen
Scala code works, but has too many `.asInstanceOf[]` injected, will have to get rid of them eventually.

What we have now:
- sets (including infinite and finite sets, including ZFC and the set of Natural Numbers)
- graphs (directed multigraphs, actually)
- categories, with parsers in the form like `val W = category"({a,b,c,d,e}, {ab: a -> b, cb: c -> b, cd: c -> d, ed: e -> d})"`
- initial and terminal objects in categories, products of categories, degreees
- functors, cones, cocones, limits, colimits
- set diagrams, that is, functors with a set category as a codomain
- diagrams (that's set diagrams) (presheaves)
- subterminals
- points
- representable functors
- (lattice of) subobjects
- build subobject classifiers,
- define Heyting trait (challenging, with generalized points),
- build logic, 
- table of logic on points,
- svg rendering of categories (see samples.html)
- classifying arrow
- Lawvere topologies

WIP:

Plans: 
- sheaves
- pullback in topos

TODO: use this trick: https://gist.github.com/non/51b83d0abc929cc4f0b153accf2bf02f

TODO: use refinement maybe: https://github.com/fthomas/refined
