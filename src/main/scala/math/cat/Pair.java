package math.cat;

/**
 * Base pair class.
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 */
public class Pair<X,Y> {
  private final X x;
  private final Y y;

  public X x() { return x;}
  public Y y() { return y;}

  private Pair(X x, Y y) {
    this.x = x;
    this.y = y;
  }

  public boolean equals(Object o) {
    return o instanceof Pair && ((Pair)o).x.equals(x) && ((Pair)o).y.equals(y);
  }

  public int hashCode() { return x.hashCode() * 2 + y.hashCode();}
  public String toString() { return "(" + x + "," + y + ")"; }

  public static <X,Y> Pair<X,Y> Pair(X x, Y y) {
    return new Pair(x, y);
  }

  public static <X> Pair<X,X> Pair(X[] source) {
    assert source.length == 2 : "Pair is built on a two-element array; got " + source.length;
    return Pair(source[0], source[1]);
  }
}

