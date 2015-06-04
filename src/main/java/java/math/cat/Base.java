package java.math.cat;

import java.util.*;


/**
 * Base tools used in this package.
 *
 * @author Vlad Patryshev
 *         All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 *         Credits: http://antilamer.livejournal.com/245962.html
 */

public class Base {

    /**
     * Builds an array of given elements.
     *
     * @param <T>      element type
     * @param elements the elements to have in the array (vararg)
     * @return the newly-built array
     */
    public static <T> T[] array(T... elements) { return elements; }

    /**
     * Given an array of keys and an array of values, builds a map
     * that maps these keys to the values.
     *
     * @param <K>    key type
     * @param <V>    value type
     * @param keys   the array of keys
     * @param values the array of values
     * @return a newly-built map
     */
    public static <K, V> Map<K, V> Map(final K[] keys, final V[] values) {
        assert keys.length == values.length;
        Map<K, V> map = new HashMap<K, V>();
        for (int i = 0; i < keys.length; i++) {
            map.put(keys[i], values[i]);
        }
        return Collections.unmodifiableMap(map);
    }

    /**
     * An empty iterator. This is a factory method,
     * to avoid type problems.
     *
     * @param <T> element type (there are no elements actually)
     * @return an iterator that has no next.
     */
    public static <T> Iterator<T> emptyIterator() {
        return new Iterator<T>() {

            public boolean hasNext() {
                return false;
            }

            public T next() {
                throw new NoSuchElementException("This iterator is empty.");
            }

            public void remove() {
                throw new NoSuchElementException("This iterator is empty.");
            }
        };
    }

    /**
     * Builds a list out of given vararg elements.
     *
     * @param <T>      element type
     * @param elements the elements
     * @return a list of all these elements
     */
    public static <T> List<T> List(T... elements) {
        return Arrays.asList(elements);
    }

    /**
     * Creates a (virtual) disjoint union set from given collections.
     * Collections are supposed not to have repeating elements; they
     * also should be disjoint. Neither of these conditions is checked.
     *
     * @param collections vararg list of participating collections
     * @return a set that contains all elements of those collections
     */
    static <A> Set<A> disjointUnion(final Collection<A>... collections) {
        return new AbstractSet<A>() {
            @Override
            public Iterator<A> iterator() {
                return new Iterator<A>() {
                    // Top iterator
                    private final Iterator<Collection<A>> i = Arrays.asList(collections).iterator();
                    // Bottom iterator
                    private Iterator<A> j = emptyIterator();

                    public boolean hasNext() {
                        for (; !j.hasNext() && i.hasNext(); j = i.next().iterator()) {
                        }
                        return j.hasNext();
                    }

                    public A next() {
                        if (hasNext()) {
                            return j.next();
                        }
                        throw new NoSuchElementException();
                    }

                    public void remove() {
                        j.remove();
                    }
                };
            }

            @Override
            public int size() {
                int size = 0;
                for (Collection<A> c : collections) {
                    size += c.size();
                    if (c.size() == Integer.MAX_VALUE || size < 0) {
                        return Integer.MAX_VALUE;
                    }
                }

                return size;
            }
        };
    }

    /**
     * Builds an inverse map. Assumes that the map is inversible.
     *
     * @param <A> map key type
     * @param <B> map value type
     * @param m   the map to invert
     * @return inverse for map m
     */
    public static <A, B> Map<B, A> inverse(Map<A, B> m) {
        Map<B, A> result = new HashMap<B, A>(m.size());
        for (Map.Entry<A, B> entry : m.entrySet()) {
            assert !result.containsKey(entry.getValue()) : "Map irreversible for " + entry;
            result.put(entry.getValue(), entry.getKey());
        }

        return result;
    }

    /**
     * Selects one of non-null elements of a given Iterable of arguments.
     *
     * @param <T>        element type
     * @param candidates the candidates to choose from
     * @return a candidate that is not null, or null if none such
     */
    public static <T> T oneOf(Iterable<T> candidates) {
        Iterator<T> i = candidates.iterator();
        return i.hasNext() ? i.next() : null;
    }

    /**
     * Selects one of non-null elements of a given vararg of arguments.
     *
     * @param <T>        element type
     * @param candidates the candidates to choose from
     * @return a candidate that is not null, or null if none such
     */
    public static <T> T oneOf(T... candidates) {
        for (T x : candidates) {
            if (x != null) {
                return x;
            }
        }
        return null;
    }

    /**
     * Builds an Iterator over elements of Cartesian product of two Iterables.
     *
     * @param <A> element type of first Iterable
     * @param <B> element type of second Iterable
     * @param x   first Iterable
     * @param y   second Iterable
     * @return the iterator that scans all possible combinations of elements of the two Iterables
     */
    static <A, B> Iterator<Pair<A, B>>
    buildProductIterator(final Iterable<A> x, final Iterable<B> y) {
        return new Iterator<Pair<A, B>>() {
            Iterator<A> i = x.iterator();
            Iterator<B> j = emptyIterator();
            A current = null;

            public boolean hasNext() {
                while (i.hasNext() && !j.hasNext()) {
                    current = i.next();
                    j = y.iterator();
                }
                return i.hasNext() || j.hasNext();
            }

            public Pair<A, B> next() {
                if (hasNext()) {
                    return BasePair.Pair(current, j.next());
                }
                throw new NoSuchElementException();
            }

            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }

    /**
     * Builds a Cartesian product of two sets.
     *
     * @param <A> element type of first set
     * @param <B> element type of second set
     * @param x   first set
     * @param y   second set
     * @return Cartesian product of two sets: the set of all possible pairs.
     */
    public static <A, B> Set<Pair<A, B>> setProduct(final Set<A> x, final Set<B> y) {
        return new AbstractSet<Pair<A, B>>() {

            @Override
            public Iterator<Pair<A, B>> iterator() {
                return buildProductIterator(x, y);
            }

            @Override
            public int size() {
                return x.size() * y.size();
            }
        };
    }

    /**
     * Flattens an Iterable of Iterables.
     *
     * @param <T>                 element type
     * @param iterableOfIterables an Iterable that lists Iterables to join
     * @return an Iterable that scans over elements of the listed Iterables
     */
    public static <T> Iterable<T> flatten(final Iterable<? extends Iterable<T>> iterableOfIterables) {
        return new Iterable<T>() {
            public Iterator<T> iterator() {
                final Iterator<? extends Iterable<T>> top = iterableOfIterables.iterator();
                assert top != null : "Top iterable's iterator() should not be null";
                return new Iterator<T>() {
                    Iterator<T> current = emptyIterator();

                    public boolean hasNext() {
                        while (!current.hasNext() && top.hasNext()) {
                            Iterable<T> nextTop = top.next();
                            assert nextTop != null : "Top level iterator should not return nulls";
                            current = nextTop.iterator();
                        }
                        return current.hasNext();
                    }

                    public T next() {
                        if (!hasNext()) {
                            throw new NoSuchElementException();
                        }
                        return current.next();
                    }

                    public void remove() {
                        current.remove();
                    }
                };
            }
        };
    }

    /**
     * Flattens a list of lists.
     *
     * @param <T>         list element type
     * @param listofLists a list of lists
     * @return a list of elements of all the inner lists, in the right order.
     */
    public static <T> List<T> flattenList(final List<? extends List<T>> listofLists) {
        return new AbstractList<T>() {
            @Override
            public T get(int index) {
                if (index < 0) {
                    throw new IndexOutOfBoundsException();
                }
                for (List<T> list : listofLists) {
                    if (index < list.size()) {
                        return list.get(index);
                    }
                    index -= list.size();
                }
                throw new IndexOutOfBoundsException();
            }

            @Override
            public Iterator<T> iterator() {
                final Iterator<? extends Iterable<T>> top = listofLists.iterator();
                assert top != null : "Top iterable's iterator() should not be null";
                return new Iterator<T>() {
                    Iterator<T> current = emptyIterator();

                    public boolean hasNext() {
                        while (!current.hasNext() && top.hasNext()) {
                            Iterable<T> nextTop = top.next();
                            assert nextTop != null : "Top level iterator should not return nulls";
                            current = nextTop.iterator();
                        }
                        return current.hasNext();
                    }

                    public T next() {
                        if (!hasNext()) {
                            throw new NoSuchElementException();
                        }
                        return current.next();
                    }

                    public void remove() {
                        current.remove();
                    }
                };
            }

            @Override
            public int size() {
                int size = 0;
                for (List<T> list : listofLists) {
                    size += list.size();
                }
                return size;
            }
        };
    }

    public static <T> List<T> flatten(final List<? extends List<T>> listofLists) {
        return flattenList(listofLists);
    }

    /**
     * Concatenates a sequence of iterables
     *
     * @param <T>       element type
     * @param iterables the iterables to concatenate
     * @return a flat iterable, listing elements of the given iterables
     */
    public static <T> Iterable<T> concat(final Iterable<T>... iterables) {
        return flatten(Arrays.asList(iterables));
    }

    /**
     * Concatenates a vararg of lists.
     *
     * @param <T>   element type
     * @param lists the lists to concatenate.
     * @return the list produced by concatenating given lists
     */
    public static <T> List<T> concat(final List<T>... lists) {
        return flattenList(Arrays.asList(lists));
    }

    /**
     * Splits an Iterable into head and tail.
     *
     * @param <T>      element type
     * @param iterable the Iterable to split
     * @return a pair consisting of the Iterable's head and tail.
     */
    public static <T> Pair<T, Iterable<T>> split(final Iterable<T> iterable) {
        Iterable<T> tail =
                new Iterable<T>() {
                    public Iterator<T> iterator() {
                        Iterator<T> tailIterator = iterable.iterator();
                        tailIterator.next();
                        return tailIterator;
                    }
                };

        return BasePair.Pair(iterable.iterator().next(), tail);
    }

    /**
     * Spits a list into head and tail pair.
     *
     * @param <T>  element type
     * @param list the list to split
     * @return a pair consisting of head and tail
     */
    public static <T> Pair<T, List<T>> split(final List<T> list) {
        return BasePair.Pair(list.get(0), list.subList(1, list.size()));
    }

    /**
     * Count entries in an Iterable.
     *
     * @param iterable the iterable to scan
     * @return number of entries in the iterable
     */
    public static int countEntries(Iterable<?> iterable) {
        int count = 0;
        for (Iterator<?> i = iterable.iterator(); i.hasNext(); i.next()) {
            count++;
        }
        return count;
    }

    /**
     * Builds a (virtual) list of integers from 0 to n-1.
     *
     * @param n number of elements in the list
     * @return the list
     */
    public static List<Integer> range(int n) {
        return range(0, n);
    }

    /**
     * Builds a (virtual) list of integers from a to b-1.
     *
     * @param a first integer
     * @param b next after the last integer
     * @return the list
     */
    public static List<Integer> range(int a, int b) {
        return range(a, b, 1);
    }

    /**
     * Builds a (virtual) list of integers from a to b-c, with a step c.
     *
     * @param a first integer
     * @param b next after the last integer
     * @param c step
     * @return the list
     */
    public static List<Integer> range(final int a, final int b, final int c) {
        return new AbstractList<Integer>() {
            @Override
            public Integer get(int index) {
                return a + index * c;
            }

            @Override
            public int size() {
                return (b - a + c - 1) / c;
            }
        };
    }

    /**
     * Zips two lists.
     *
     * @param <A> element type of first list
     * @param <B> element type of second list
     * @param as  first list
     * @param bs  second list
     * @return the list of pairs of parallel elements of as and bs.
     */
    public static <A, B> List<Pair<A, B>> zip(final List<A> as, final List<B> bs) {
        return new AbstractList<Pair<A, B>>() {
            @Override
            public Pair<A, B> get(int index) {
                return BasePair.Pair(as.get(index), bs.get(index));
            }

            @Override
            public int size() {
                return Math.min(as.size(), bs.size());
            }
        };
    }

    /**
     * Checks if two objects are equal.
     *
     * @param a first object
     * @param b second object
     * @return true if objects are equal, in Java sense
     */
    protected static boolean equal(Object a, Object b) {
        return a == b || a != null && a.equals(b);
    }

    /**
     * @{inheritDoc}
     */
    protected static int hashCode(Object a) {
        return a == null ? 0 : a.hashCode();
    }

    /**
     * Builds a map that returns a list element by its index.
     *
     * @param <X>  element type
     * @param list the list
     * @return the
     */
    public static <X> Map<Integer, X> toMap(final List<X> list) {
        return new Functions.Function<Integer, X>() {
            @Override
            public X apply(Integer i) {
                return list.get(i);
            }
        }.toMap(Sets.numbers(list.size()));
    }

    /**
     * Builds a (virtual) identity map from a set to itself
     *
     * @param set set on which identity map is defined
     * @return the identity map
     * hmm... maybe the stuff in Functions makes more sense?
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
    }*/
}
