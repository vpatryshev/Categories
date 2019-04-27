package math.cat.topos

import math.Base
import math.Base._
import math.cat._
import math.sets.Functions._
import math.sets.Sets.{filter, set, _}
import math.sets.{FactorSet, Sets}
import scalakittens.{Good, Result}

import scala.language.postfixOps

/**
  * Diagram from a category to Categories.SETF.
  *
  * The term "point" below means a point in categorical meaning:
  * an arrow from a terminal object into a given object.
  * Here we talk about the category of diagrams, so a point is a
  * singleton diagram. It must have been a mapping from objects of the base category to values
  * that are elements of the sets: given a diagram D, p(x) \in D(x).
  */
abstract class Diagram(
  tag: String,
  override val d0: Category)
  extends Functor(tag, d0, SetCategory.Setf) { diagram =>
  type XObject = d0.Obj
  type XObjects = Set[d0.Obj]
  type XArrow = d0.Arrow
  type XArrows = Set[d0.Arrow]

  implicit def asSet(x: d1.Obj): set = x.asInstanceOf[set]
  
  def ⊂(other: Diagram): Boolean = {
    val itsok = d0.objects.forall { o ⇒
      this(o) subsetOf other(o)
    }
    itsok
  }

  def ∈(other: Diagram): Boolean = {
    val itsok = d0.objects.forall { o ⇒
      other(o)(this(o))
    }
    itsok
  }

  implicit def asFunction(a: d1.Arrow): SetFunction = a.asInstanceOf[SetFunction]
  // for each original object select a value in the diagram
  // not necessarily a point; must be compatible
  // something like Yoneda embedding, but not exactly

  def point(mapping: d0.Obj => Any): Point = new Point {

    override val d0: Category = diagram.d0

    def at(x: d0.Obj): Unit = mapping(diagram.d0.obj(x))
  }


  def apply(x: Any): set = asSet(objectsMapping(d0.obj(x)))

  /**
    * Calculates this diagram's limit
    *
    * @return this functor's limit
    */
  override def limit: Result[Cone] = {
    val bundleObjects: XObjects = limitBuilder.bundles.keySet

    def arrowsFromBundles(obj: XObject): XArrows = limitBuilder.bundles.get(obj).toSet.flatten

    // For each object of domain we have an arrow from one of the objects used in building the product
    val arrowsInvolved: XArrows =
      bundleObjects flatMap (bo ⇒ arrowsFromBundles(bo)) filterNot d0.isIdentity

    val fromRootObjects: Map[XObject, XArrow] =
      arrowsInvolved.groupBy(arrow ⇒ d0.d1(arrow)).mapValues(_.head) // does not matter which one, in this case

    def arrowFromRootObject(x: d0.Obj) =
      if (limitBuilder.rootObjects(x)) d0.id(x) else fromRootObjects(x)

    val vertex = limitBuilder.vertex

    def coneMap(x: d0.Obj): d1.Arrow = d1.arrow {
      val arrowToX: XArrow = arrowFromRootObject(x)
      val rootObject: XObject = d0.d0(arrowToX)
      val f: SetFunction = arrowsMapping(arrowToX)
      val projections: List[Any] ⇒ Any = limitBuilder.projectionForObject(rootObject)
      SetFunction.build(s"vertex to ($tag)[$x]", vertex, f.d1,
        { case point: List[Any] ⇒ f(projections(point)) }) iHope // what can go wrong?
    }
    //YObjects vertex
    Good(Cone(d1.obj(limitBuilder.vertex), coneMap))
  }

  override def colimit: Result[Cocone] = {
    val op = d0.op
    val participantArrows: Set[op.Arrow] = op.arrowsFromRootObjects // filterNot domain.isIdentity
    // for each object, a set of arrows starting at it object
    val bundles: XObject ⇒ XArrows =
    d0.buildBundles(d0.objects, participantArrows.asInstanceOf[XArrows])
    val listOfObjects: List[XObject] = op.listOfRootObjects.asInstanceOf[List[XObject]]
    // Here we have a non-repeating collection of sets to use for building a union
    val setsToJoin: List[Set[Any]] = listOfObjects map nodesMapping map asSet
    val union: DisjointUnion[Any] = DisjointUnion(setsToJoin)
    val typelessUnion: set = union.unionSet untyped
    val directIndex: IntMap[XObject] = Base.toMap(listOfObjects)
    val reverseIndex: Map[XObject, Int] = Base.inverse(directIndex)

    // for every object it gives the inclusion of the image of this object into the union
    val objectToInjection: Map[XObject, Injection[Any, (Int, Any)]] =
      reverseIndex mapValues union.injection

    // All possible functions in the diagram, bundled with domain objects
    val functionsToUnion: Set[(XObject, SetFunction)] = for {
      o <- d0.objects
      a <- bundles(o)
      from: set = nodesMapping(o)
      aAsMorphism: SetFunction = arrowsMapping(a)
      embeddingToUnion =
      SetFunction.build("in", aAsMorphism.d1, typelessUnion, objectToInjection(d0.d1(a))).iHope
      g = aAsMorphism.andThen(embeddingToUnion) // do we need it?
    } yield (o, g)

    // Account for all canonical functions
    val canonicalFunctionPerObject: Map[XObject, SetFunction] =
      functionsToUnion.toMap

    val theFactorset: factorset = new FactorSet(typelessUnion)

    // have to factor the union by the equivalence relationship caused
    // by two morphisms mapping the same element to two possibly different.
    for (o <- d0.objects) {
      val F_o = nodesMapping(o) // the set to which `o` maps
      val arrowsFrom_o: Seq[XArrow] = bundles(o).toList

      def inclusionToUnion(a: XArrow): Any ⇒ Any = {
        arrowsMapping(a).mapping andThen objectToInjection(d0.d1(a))
      }

      arrowsFrom_o match {
        case a0 :: tail ⇒
          val f = inclusionToUnion(a0)
          for (a <- tail) {
            val g = inclusionToUnion(a)
            for (x <- F_o) {
              theFactorset.merge(f(x), g(x))
            }
          }
        case other ⇒ // do nothing
      }
    }
    val factorMorphism: SetFunction = SetFunction.forFactorset(theFactorset)

    def coconeMap(x: XObject): d1.Arrow = d1.arrow {
      canonicalFunctionPerObject(x) andThen factorMorphism
    }

    Good(Cocone(d1.obj(theFactorset.content.untyped), coconeMap))
  }

  def points: Iterable[Diagram] = {
    val listOfObjects: List[XObject] = domainObjects.toList
    val listOfComponents: List[set] = listOfObjects map objectsMapping map asSet

    def isCompatible(om: PointLike[XObject]) = d0.arrows.forall {
      a ⇒
        val d00 = om(d0.d0(a))
        val d01 = om(d0.d1(a))
        val f = arrowsMapping(a)
        val itsok = f(d00) == d01

        itsok
    }

    val objMappings = for {
      values <- Sets.product(listOfComponents) //.view
      om: PointLike[XObject] = listOfObjects zip values toMap;
      if isCompatible(om)
    } yield om
    
    val sorted = objMappings.toList.sortBy(_.toString.replace("}", "!")).zipWithIndex

    val all = sorted map {
      case (map, i) ⇒
        def om(o: d0.Obj): set = asSet(d1.obj(singleton(map(o))))

        def am(a: d0.Arrow): SetFunction = {
          val original = arrowsMapping(a)
          val newDomain = om(d0.d0(a))
          val newCodomain = om(d0.d1(a))
          val setFunction = original.restrictTo(newDomain, newCodomain)
          setFunction iHope
        }
        val tag = s"#${i + 1}"
        val d = Diagram.build(tag, d0)(om, am)
        if (d.isBad) {
          val d1 = Diagram.build(tag, d0)(om, am)
        }
        d
    }
    all collect {
      case Good(diagram) ⇒ diagram
    }
  }

  def subobjects: Iterable[Diagram] = {
    val allSets: Map[d0.Obj, set] = domainObjects map (o ⇒ o → toSet(objectsMapping(o))) toMap
    val allPowers: Map[d0.Obj, Set[set]] = allSets.mapValues(Sets.powerset)

    val listOfObjects: List[d0.Obj] = domainObjects.toList
    val listOfComponents: List[Set[set]] = listOfObjects.map(allPowers)

    def isCompatible(om: PointLike[XObject]) = d0.arrows.forall {
      a ⇒
        val d00 = toSet(om(d0.d0(a)))
        val d01 = om(d0.d1(a))
        val f = arrowsMapping(a)
        val itsok = (d00 map f, d01) match {
          case (s0: Set[Any], s1: Set[Any]) ⇒ s0 subsetOf s1
          case (x, y) ⇒ 
            x == y
        }

        itsok
    }

    val objMappings = for {
      values <- Sets.product(listOfComponents) //.view
      om: PointLike[XObject] = listOfObjects zip values toMap;
      if isCompatible(om)
    } yield om
    val sorted = objMappings.toList.sortBy(_.toString)

    def am(a: d0.Arrow): SetFunction = {
      val dom: Set[set] = allPowers(d0.d0(a))
      val codom: Set[set] = allPowers(d0.d1(a))
      val f = arrowsMapping(a)

      SetFunction.build("", toSet(dom), toSet(codom), x ⇒ toSet(x) map f).iHope
    }

    var i = 0
    val allCandidates = sorted map {
      case om: Map[d0.Obj, Sets.set] ⇒
        i += 1
        def am(a: d0.Arrow): SetFunction = {
          val dom: Sets.set = om(d0.d0(a))
          val codom: Sets.set = om(d0.d1(a))
          SetFunction.build("", dom, codom, arrowsMapping(a)).iHope
        }
        Diagram.build(s".$i", d0)(om, am)
    }
    
    val goodOnes = allCandidates.collect { case Good(d) ⇒ d}
    goodOnes
  }

  override def toString = s"Diagram[${d0.name}](${
    d0.objects map { x ⇒ x + "→{" + objectsMapping(x).mkString(",") + "}" } mkString ", " replace(s"Diagram[${d0.name}]", "")
  })".replace("Set()", "{}")

  /**
    * Builds a predicate that checks if a given set of arrows map a given element of Cartesian product to the same value
    *
    * @param point element of Cartesian product
    * @return the predicate
    */
  private[cat] def allArrowsAreCompatibleOnPoint(point: Point): XArrows ⇒ Boolean =
    arrows ⇒ arrows.forall(f ⇒ arrows.forall(g ⇒ {
      arrowsAreCompatibleOnPoint(point)(f, g)
    }))



  private[cat] def allArrowsAreCompatibleOnPoint(point: PointLike[XObject]): XArrows ⇒ Boolean =
    arrows ⇒ arrows.forall(f ⇒ arrows.forall(g ⇒ {
      arrowsAreCompatibleOnPoint(point)(f, g)
    }))

  /**
    * Checks whether two arrows action on a given point produce the same element. 
    *
    * @param point a point in the diagram
    * @param f     first arrow
    * @param g     second arrow
    * @return true if they are
    */
  private[cat] def arrowsAreCompatibleOnPoint(point: Point)(f: XArrow, g: XArrow): Boolean = {
    val f_x = arrowActionOnPoint(f, point)
    val g_x = arrowActionOnPoint(g, point)
    f_x == g_x
  }



  private[cat] def arrowsAreCompatibleOnPoint(point: PointLike[XObject])(f: XArrow, g: XArrow): Boolean = {
    val f_x = arrowActionOnPoint(f, point)
    val g_x = arrowActionOnPoint(g, point)
    f_x == g_x
  }

  /**
    * Calculates the action of a given arrow on a point of a diagram.
    *
    * @param a     the arrow
    * @param point a mapping in the sets corresponding to objects from the list above
    * @return the value to which the arrow maps a component of the point
    */
  private def arrowActionOnPoint(a: XArrow, p: Point): Any = {
    val aDomain: d0.Obj = d0.d0(a)
    arrowsMapping(a)(p at p.d0.obj(aDomain))
  }


  private def arrowActionOnPoint(a: XArrow, point: PointLike[XObject]): Any =
    arrowsMapping(a)(point(d0.d0(a)))

  private[cat] def toSet(x: Any): set = asSet(d1.obj(x))

  private[cat] object limitBuilder {
    // have to use list so far, no tool to annotate cartesian product components with their appropriate objects
    final private[cat] lazy val listOfObjects: List[XObject] = rootObjects.toList.sortBy(_.toString)
    // Here we have a non-repeating collection of sets to use for building a limit
    final private[cat] lazy val setsToUse = listOfObjects map nodesMapping map asSet
    // this is the product of these sets; will have to take a subset of this product
    final private[cat] lazy val prod: Set[List[Any]] = product(setsToUse)
    final lazy private[cat] val cobundles: Map[XObject, XArrows] = d0.op.buildBundles(opo, opa)
      .asInstanceOf[Map[XObject, XArrows]]
    // this is the limit object
    final private[cat] lazy val vertex: set = prod filter isPoint untyped
    // bundles maps each "initial" object to a set of arrows from it
    final private[cat] lazy val bundles: Map[d0.Obj, XArrows] =
      d0.buildBundles(rootObjects, participantArrows)
    lazy val rootObjects: XObjects = d0.allRootObjects.asInstanceOf[XObjects] // same thing
    private lazy val participantArrows: XArrows = d0.arrowsFromRootObjects
    // for each domain object, a collection of arrows looking outside
    private val opo: d0.op.Objects = d0.op.objects
    private val opa: d0.op.Arrows = participantArrows.asInstanceOf[d0.op.Arrows]

    // this function takes an object and returns a projection set function;
    // we have to compose each such projection
    // with the right arrow from root object to the image of our object
    private[cat] def projectionForObject(x: XObject)(xs: List[Any]): Any = {
      val i = index(x)
      xs(i)
    }

    private def index(x: XObject): Int = listOfObjects.indexOf(x)

    // Have a product set; have to remove all the bad elements from it
    // this predicate leaves only compatible elements of product (which are lists)
    private[cat] def isPoint(candidate: List[Any]): Boolean = {
      val p: Point = point(listOfObjects zip candidate toMap)
      val checkCompatibility = allArrowsAreCompatibleOnPoint(p)
      
      val pl: PointLike[XObject] = listOfObjects zip candidate toMap
      val checkCompatibilityL = allArrowsAreCompatibleOnPoint(pl)
      val arrowSets = cobundles.values
      val setsToCheck = arrowSets filterNot (_.forall(d0.isIdentity))
      setsToCheck.forall(checkCompatibilityL)
    }
  }

  type PointLike[Z] = Map[Z, Any]
  
  trait PointLikel extends Point with PointLike[d0.Obj] {
    val d0: Category = diagram.d0
  }
}

object Diagram {

  def build(tag: String, domain: Category)(
    objectsMap: domain.Obj ⇒ set,
    arrowMap: domain.Arrow ⇒ SetFunction): Result[Diagram] = {

    val diagram: Diagram = new Diagram(tag, domain) {
      override val objectsMapping: d0.Obj ⇒ d1.Obj =
        (x: d0.Obj) ⇒ d1.obj(objectsMap(domain.obj(x)))

      override val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow =
        (a: XArrow) ⇒ d1.arrow(arrowMap(domain.arrow(a)))
    }

    Functor.validateFunctor(diagram) returning diagram
  }
}

trait Point {
  val d0: Category

  def ∈(other: Diagram): Boolean = {
    val itsok = d0.objects.forall { o ⇒
      other(o)(at(o))
    }
    itsok
  }

  def at(x: d0.Obj): Any // todo: make it type-safe
}
