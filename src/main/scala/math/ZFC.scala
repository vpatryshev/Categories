package math

import java.util
import java.util.Comparator

import scala.collection.mutable.ListBuffer

class ZFC {
  val PATIENCE = 100

  def log(s: String): Unit = {
    // no logging by default
  }

  private val domain = new ListBuffer[SetZ]

  private def register(o: SetZ): SetZ = {
    log("exists " + o)
    if (!domain.contains(o)) {
      domain.append(o)
    }
    o
  }

  type Predicate = Any => Boolean

  private def exists(s: SetZ) = register(s)

  private def exists(p: Predicate): Boolean = domain exists p

  private def forall(p: Predicate) = domain forall p

  // Axiom I (extensionality)
  private def equal(a: SetZ, b: SetZ) =
    (a eq b) || forall(x => a.contains(x) == b.contains(x))

  def intersection(a: SetZ, b: SetZ): SetZ = exists(
    new SetZ(s"${a.id} ∩ ${b.id}", x => a.contains(x) && b.contains(x)
  ))

  class SetZ(val id: String, val thePredicate: Predicate) {
    
    def denote(id: String): SetZ = new SetZ(id, thePredicate) {
      log(id + "=" + this)
    }

    def contains: Predicate = thePredicate

    override def equals(o: Any): Boolean = o match {
      case sz: SetZ => equal(this, sz)
      case otherwisse => false
    }

    def isSubsetOf(s: SetZ): Boolean = (this eq s) || forall(s.contains)

    def choose1: Option[Any] = domain headOption

    override def toString: String = toString(PATIENCE)

    def toString(patience: Int): String = {
      if (id != null) return id
      
      def stringify(x: Any, limit: Int) = x match {
        case sz: SetZ => sz.toString(limit)
        case any => any.toString
      }
      
      val (_, content) = ((0, List.empty[String]) /: domain.filter(thePredicate)) {
        case ((n, list), x) =>
          val s = stringify(x, patience - n)
          (n+s.length, s::list)
      }
      
      val sb = new StringBuilder("{")
      for (s <- content.sorted takeWhile (_ => sb.length < patience)) {
        if (sb.length > 1) sb.append(",")
        sb.append(s)
      }
      if (sb.length > patience) {
        sb.append("...")
      }
      sb.append("}")
      sb.toString
    }

    // Axiom II (empty set)
    val EMPTY: SetZ = exists(new SetZ("∅", _ => false))

    // Axiom III (comprehension)
    def comprehension(p: Predicate, s: SetZ): SetZ =
      exists(new SetZ(s"{...}",(x: Any) => p(x) && s.contains(x)))

    // Axiom IV (powerset)
    def powerset(a: SetZ): SetZ =
      exists(new SetZ(s"P(${a.id})",
        {
          case b: SetZ => b isSubsetOf a
          case other => false
        }))

    // Axiom V (union)
    def union(s: SetZ): SetZ = exists(new SetZ(s"∪${s.id}",
      x => exists({
        case y: SetZ => s.contains(y) && y.contains(x)
        case otherwise => false
      })))

    // Axiom VI choice
//    def choice(s: SetZ): SetZ = exists(new SetZ(s"choice of ${s.id}",
//      (x: Any): Boolean = exists(new Predicate() { // existence
//        override def eval(y: Any): Boolean = return s.contains(y) && (x eq y.asInstanceOf[SetZ].choose1)
//      })
//    })
  }

}
