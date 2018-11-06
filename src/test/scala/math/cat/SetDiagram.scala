package math.cat

/**
  * Diagram from a category to Categories.SETF.
  *
  * The term "point" below means a point in categorical meaning:
  * an arrow from a terminal object into a given object.
  * Here we talk about the category of diagrams, so a point is a
  * singleton diagram. It must have been a mapping from objects of the base category to values
  * that are elements of the sets: given a diagram D, p(x) \in D(x).
  *
  * @author Vlad Patryshev
  *         All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
  * @tparam Objects type of objects of domain category
  * @tparam Arrows  type of arrows of domain category
  */
class SetDiagram[Objects, Arrows](
                                   tag: String,
                                   domain: Category[Objects, Arrows],
                                   objectsMorphism: SetMorphism[Objects, Set[Any]],
                                   arrowsMorphism: SetMorphism[Arrows, SetFunction])

  extends Functor[Objects, Arrows, Set[Any], SetFunction](
    tag, domain, SetCategory.Setf, objectsMorphism, arrowsMorphism) {

  // for each original object select a value in the diagram
  // not necessarily a point; must be compatible
  type PointLike = Map[Objects, Any]

  /**
    * Builds a predicate that checks if a given set of arrows map a given element of Cartesian product to the same value
    *
    * @param point element of Cartesian product
    * @return the predicate
    */
  private[cat] def allArrowsAreCompatibleOnPoint(point: PointLike): Set[Arrows] => Boolean =
    (setOfArrows: Set[Arrows]) => {
      setOfArrows.forall(f => setOfArrows.forall(g => arrowsAreCompatibleOnPoint(point)(f, g)))
    }

  /**
    * Checks whether two arrows action on a given point produce the same element. 
    *
    * @param point a point in the diagram
    * @param f     first arrow
    * @param g     second arrow
    * @return true if they are
    */
  private[cat] def arrowsAreCompatibleOnPoint(point: PointLike)(f: Arrows, g: Arrows): Boolean = {
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
  private def arrowActionOnPoint(a: Arrows, point: PointLike): Any =
    arrowsMorphism(a)(point(domain.d0(a)))

  private[cat] class Limit {
    private val participantObjects = domain.allRootObjects
    private val participantArrows = domain.arrowsFromRootObjects
    // have to use list so far, no tool to annotate cartesian product components with their appropriate objects
    final private[cat] val listOfObjects = participantObjects.toList
    private def index(x: Objects): Int = listOfObjects.indexOf(x)
    
    // this function takes an object and returns a projection set function; we have to compose each such projection
    // with the right arrow from important object to the image of our object
    private[cat] def projectionForObject(x: Objects)(xs: List[Objects]): Objects = xs(index(x))

    // Here we have a non-repeating collection of sets to use for building a limit
    private[cat] val setsToUse = listOfObjects map nodesMorphism
    // this is the product of these sets; will have to take a subset of this product
    private[cat] val product: Set[List[Any]] = Sets.product(setsToUse)
    // for each domain object, a collection of arrows looking outside
    final private[cat] val cobundles: SetMorphism[Objects, Set[Arrows]] =
      domain.op.buildBundles(domain.objects, participantArrows)
    // Have a product set; have to remove all the bad elements from it
    // this predicate leaves only compatible elements of product (which are lists)
    private[cat] def isPoint(candidate: List[Any]): Boolean = {
      val point: PointLike = listOfObjects zip candidate toMap
      val checkCompatibility = allArrowsAreCompatibleOnPoint(point)
      cobundles.values.forall(checkCompatibility)
    }
    // this is the limit object
    final private[cat] val apex: Set[Any] = product filter isPoint map identity

    // bundles maps each "initial" object to a set of arrows from it
    final private[cat] val bundles: SetMorphism[Objects, Set[Arrows]] =
      domain.buildBundles(participantObjects, participantArrows)
  }

  /**
    * Calculates this diagram's limit
    * @return this functor's limit
    */
  override def limit: Option[Cone] = {
    val data: Limit = new Limit
    // For each object of domain we have an arrow from one of the objects used in building the product

    val arrowsInvolved = for {
      obj <- data.bundles.keySet
      arrow <- data.bundles.get(obj).toSet.flatten
    } yield arrow

    val arrowFromImportantObject: Map[Objects, Arrows] =
      arrowsInvolved.groupBy(domain.d1).mapValues(_.head) // does not matter which one, in this case
    
    // takes an object, returns an arrow from an important object to the image of object in SETF
    def setFunctionForObject(obj: Objects): SetFunction = arrowsMorphism(arrowFromImportantObject(obj))
    def coneMap(x: Objects): SetFunction = {
      val f: SetFunction = setFunctionForObject(x)
      SetFunction("apex to F[x]", data.apex, f.d1,
        { case point: List[Objects] => 
          val obj = data.projectionForObject(x)(point)
          val arrow = arrowFromImportantObject(obj)
          arrowsMorphism(arrow).function})
      }
    //YObjects apex
    Option(Cone(data.apex, coneMap))
  }

}
