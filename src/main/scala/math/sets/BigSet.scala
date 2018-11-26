package math.sets

/**
  * An implementation of a large Set, can be non-enumerable.
  * AC is also optional here.
  *
  * @tparam T element type
  */
abstract class BigSet[T] extends Set[T] {
  override def size: Int = Sets.InfiniteSize

  def whoami: String = "A BIG SET"

  override def toString: String = whoami

  def map[U](f: Functions.Bijection[T, U]): BigSet[U] = BigSet((u: U) => this contains (f unapply u))

  override def filter(p: T => Boolean): BigSet[T] = BigSet((t: T) => p(t) && (this contains t))

  override def hashCode: Int = System.identityHashCode(this)

  /**
    * This is a pretty bad equality for sets, but what can we do?
    * @param other anything, according to Java rules
    * @return true if it is the same set
    */
  override def equals(other: Any): Boolean = other match {
    case ref: AnyRef => this eq ref
    case notaref => false
  }
}

object BigSet {
  def apply[T](set: Set[T]): BigSet[T] = new BigSet[T] with EnumerableSet[T] {
    override def contains(t: T) = set(t)
    override def size: Int = set.size
    override def toString: String = set.toString
    override def iterator: Iterator[T] = set.iterator
  }

  def apply[T](p: T => Boolean = (_: T) => true): BigSet[T] =
    new BigSet[T] with NonEnumerableSet[T] {
      override def contains(t: T) = p(t)
    }

}