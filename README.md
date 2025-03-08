## Categories Project

### Intro

This is a Scala implementation of category theory calculations and topos logic (and topologies).
Historically, it was first written in Basic, then in Fortran, then in Assembler, then in Java, now in Scala.
Long story.

The idea is to be able to do all categorical calculations, then be able to calculate topos logic and topologies.

A bunch of slide sets can be found here:
- [Calculating Intuitionistic Logic in Scala](https://tinyurl.com/lambda19cils)
- [Topologies in Intuitionistic Logic](https://tinyurl.com/lambda20topologies)

### Current Status
Everything works, but has too many `.asInstanceOf[]` injected, will have to get rid of them eventually.

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
- pullback in a topos
- Scala 3

TODO: use this trick: https://gist.github.com/non/51b83d0abc929cc4f0b153accf2bf02f
