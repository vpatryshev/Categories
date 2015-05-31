package math.cat;

import java.util.*;


/**
 * Base tools used in this package.
 *
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 *
 * Credits: http://antilamer.livejournal.com/245962.html
 */

public class Base {
  public static <T> Set<T> Set(T... elements) {
    return new HashSet<T>(Arrays.asList(elements));
  }

  public static <T> T[] array(T... elements) {
    return elements;
  }

  public static <K,V> Map<K,V> Map(final K[] keys, final V[] values) {
    assert keys.length == values.length;
    Map<K,V> map = new HashMap<K,V>();
    for (int i = 0; i < keys.length; i++) {
      map.put(keys[i], values[i]);
    }
    return Collections.unmodifiableMap(map);
  }

  /**
   * Extracts a set from a string as produced by Set.toString().
   * Entries in the set can not be empty strings and cannot contain commas.
   *
   * @param s the string
   * @return restored set
   */
  public static Set<String> parseSet(String s) {
    String[] content = s.substring(1, s.length() - 1).split(",\\s*");
    Set<String> result = new HashSet<String>();
    for (String entry : content) {
      String trimmed = entry.trim();
      if (!trimmed.isEmpty()) result.add(trimmed);
    }
    return result;
  }

  /**
   * Builds a (virtual) identity map from a set to itself
   * @param set set on which identity map is defined
   * @return the identity map
   */
  public static <T> Map<T, T> id(final Set<T> set) {
    return new AbstractMap<T, T>() {

      public Set<Entry<T, T>> entrySet() {
        return new AbstractSet<Entry<T, T>>() {

          public Iterator<Entry<T, T>> iterator() {
            return new Iterator<Entry<T, T>>() {
              Iterator<T> i = set.iterator();
              T currentValue;
              Entry<T, T> currentEntry = new Entry<T, T>() {
                public T getKey() {
                  return currentValue;
                }

                public T getValue() {
                  return currentValue;
                }

                public T setValue(T value) {
                  throw new UnsupportedOperationException();
                }
              };

              public boolean hasNext() {
                return i.hasNext();
              }

              public Entry<T, T> next() {
                currentValue = i.next();
                return currentEntry;
              }

              public void remove() {
                i.remove();
              }
            };
          }

          public int size() {
            return set.size();
          }
        };
      }
    };
  }
}
