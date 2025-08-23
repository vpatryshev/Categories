package math.sets

import scalakittens.Container
import scalakittens.Containers.*
import scalakittens.Params.verbose

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.annotation.targetName

object ZFC:
  private val PATIENCE = 100

  /**
    * A cache for storing sets that are built in the process
    */
  private val domain = new ListBuffer[SetZ]

  /**
    * Adds another set to cache
    * @param o a set to add to cache
    * @return the added set
    */
  private def register(o: SetZ): SetZ =
    verbose(s"exists $o")
    if !(domain contains o) then domain.append(o)
    o

  type Predicate = Any => Boolean

  /**
    * Notifies the world that a set exists (and is registered)
    * @param aSet set that exists
    * @return the same set
    */
  private def exists(aSet: SetZ) = register(aSet)

  /**
    * Checks whether a set exists (and is registered) that satisfies a predicate
    * @param p the predicate
    * @return true if such a set exists
    */
  private def exists(p: Predicate): Boolean = domain exists p

  /**
    * Checks that all known (registered) sets satisfy a predicate
    * @param p the predicate
    * @return true if they do
    */
  private def forall(p: Predicate) = domain forall p

  // Axiom I (extensionality)
  private def equal(a: SetZ, b: SetZ) =
    (a eq b) || forall(x => (x ∈ a) == (x ∈ b))

  def intersection(a: SetZ, b: SetZ): SetZ = exists(
    new SetZ(s"${a.id} ∩ ${b.id}", x => a.contains(x) && b.contains(x)
  ))

  /**
    * Class of sets in ZF(C)
    * @param id an id of a set in ZFC
    * @param thePredicate filtering the set's elements
    */
  class SetZ(val id: String, val thePredicate: Predicate):

    /**
      * Assigns a name to this set
      * @param theId the name
      * @return a new set, same elements, but with a new name
      */
    def denote(theId: String): SetZ =
      new SetZ(theId, thePredicate):
        verbose(id + "=" + this)

    def contains: Predicate = thePredicate

    override def equals(o: Any): Boolean = o match
      case sz: SetZ => equal(this, sz)
      case otherwise => false

    private infix def isSubsetOf(s: SetZ): Boolean = (this eq s) || forall(s.contains)

    def choose1: Option[Any] = domain headOption

    override lazy val toString: String = toString(PATIENCE)

    private def toString(patience: Int): String = id

    // Axiom II (empty set)
    val EMPTY: SetZ = exists(new SetZ("∅", _ => false))

    // Axiom III (comprehension)
    def comprehension(p: Predicate, s: SetZ): SetZ =
      exists(new SetZ(s"{...}",(x: Any) => p(x) && s.contains(x)))

    // Axiom IV (powerset)
    def powerset(a: SetZ): SetZ =
      exists(new SetZ(s"P(${a.id})",
        {
          case b: SetZ => b.isSubsetOf(a)
          case other => false
        }))

    // Axiom V (union)
    def union(s: SetZ): SetZ = exists(new SetZ(s"∪${s.id}",
      x => exists({
        case y: SetZ => s.contains(y) && y.contains(x)
        case otherwise => false
      })))

  extension [T](x: T)
    @targetName("in")
    infix inline def ∈(X: SetZ): Boolean = X contains x
    @targetName("notIn")
    infix inline def ∉(X: SetZ): Boolean = !(X contains x)

    // Axiom VI choice
//    def choice(s: SetZ): SetZ = exists(new SetZ(s"choice of ${s.id}",
//      (x: Any): Boolean = exists(new Predicate() { // existence
//        override def eval(y: Any): Boolean = return s.contains(y) && (x eq y.asInstanceOf[SetZ].choose1)
//      })
//    })
