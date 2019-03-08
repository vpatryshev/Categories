package j.math.cat;

import java.util.AbstractSet;
import java.util.Iterator;

/**
 * Natural numbers class.
 */
public class N extends AbstractSet<Integer> {
  @Override
  public Iterator<Integer> iterator() {
    return new Iterator<Integer>() {
      int n = 0;

      public boolean hasNext() {
        return true;
      }

      public Integer next() {
        return n++;
      }

      public void remove() {
        throw new UnsupportedOperationException();
      }
    };
  }

  @Override
  public int size() {
    return Integer.MAX_VALUE;
  }
}
