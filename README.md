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

Plans: presheaves, sheaves, subobject classifiers, logic, Grothendieck topologies
