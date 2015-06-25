package j.math.cat;

/**
 * Base maybe monad class. Functionally, the option of having null as a value is the same. 
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 */
public class Maybe<T> {
  private final T value;
  private final boolean hasValue;

  private Maybe(T value) {
    this.value = value;
    this.hasValue = true;
  }

  private Maybe() {
    this.value = null;
    this.hasValue = false;
  }

  public T value() {
    if (hasValue) return value;
    throw new IllegalStateException("No value in this instance.");
  }

  public boolean hasValue() { return hasValue; }

  public int hashCode() { return hasValue() ? value.hashCode() : 0; }

  public boolean equals(Object o) {
    if (o instanceof Maybe) {
      Maybe<?> other = (Maybe)o;
      return hasValue() == other.hasValue() &&
          (hasValue() ? value().equals(other.value()) : true);
    }
    return false;
  }

  public String toString() {
    return hasValue() ? value.toString() : "NONE";
  }

  @SuppressWarnings({"rawtypes","unchecked"})
  public final static Maybe NONE = new Maybe();

  @SuppressWarnings({"rawtypes","unchecked"})
  public static <T> Maybe<T> some(Maybe<T> maybe) {
    return maybe == null ? NONE : maybe;
  }

  @SuppressWarnings({"unchecked"})
  public static <T> Maybe<T> some(T value) {
    return value == null ? NONE : new Maybe<T>(value);
  }
}
