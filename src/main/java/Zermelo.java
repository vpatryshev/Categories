import java.util.List;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Arrays;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.LinkedList;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.Comparator;

/**
 * http://en.wikipedia.org/wiki/Zermelo_set_theory
 *
 * Note that I use 'boolean', but I mean logical (intuitionistic).
 */
@SuppressWarnings({"rawtypes","unchecked"})
public class Zermelo {
  int PATIENCE = 100;
  public void log(String s) {
    // no logging by default
  }

  private Map<Object, Singleton> singletonHash =
      new IdentityHashMap<Object, Singleton>();
  // There's a little bit of a problem here. Domain contains
  // only sets. Should contain everything.
  private List domain = new ArrayList();
  private Object register(Object o) {
    log("exists " + o);
    for(int i = 0; i < domain.size(); i++) {
      Object t = domain.get(i);
      if (t.equals(o)) return t;
    }
    domain.add(o);
    return o;
  }

  interface Predicate {
    boolean eval(Object x);
  }

  public SetZ existsSingletonFor(Object x) {
    if (singletonHash.containsKey(x)) return singletonHash.get(x);
    Singleton s = new Singleton(x);
    singletonHash.put(x, s);
    register(x);
    return exists(s);
  }

  private SetZ exists(SetZ s) {
    return (SetZ)register(s);
  }

  private boolean exists(Predicate p) {
    for (int i = 0; i < domain.size(); i++) {
      if (p.eval(domain.get(i))) return true;
    }

    return false;
  }

  private boolean foreach(Predicate p) {
    boolean ok = true;
    int size = domain.size(); // ignore new stuff here
    for (int i = 0; ok && i < size; i++) {
      ok = p.eval(domain.get(i));
    }

    return ok;
  }

  // Axiom I (extensionality)
  private boolean equal(final SetZ a, final SetZ b) {
    return a == b ||
      foreach(new Predicate() {
        public boolean eval(Object x) {
          boolean isInA = a.contains(x);
          boolean isInB = b.contains(x);
          return isInA == isInB;
//          return a.contains(x) == b.contains(x);
        }
      }
    );
  }

  public SetZ intersection(final SetZ a, final SetZ b) {
    return exists(new SetZ() {
      public boolean contains(Object x) {
        return a.contains(x) && b.contains(x);
      }
    });
  }

  abstract class SetZ {
    abstract boolean contains(Object x);

    Predicate thePredicate = new Predicate() {
      public boolean eval(Object x) {
        return contains(x);
      }
    };

    String id = null;

    public SetZ denote(String id) {
      log(id + "=" + this);
      this.id = id;
      return this;
    }

    public Predicate contains() {
      return thePredicate;
    }

    public boolean equals(Object o) {
      return o instanceof SetZ && equal(this, (SetZ)o);
    }

    private boolean foreach(Predicate p) {
      boolean ok = true;
      for (int i = 0; ok && i < domain.size(); i++) {
        Object x = domain.get(i);
        if (contains(x)) ok = p.eval(x);
      };
      return ok;
    }

    public boolean isSubsetOf(final SetZ s) {
      return this == s || foreach(s.contains());
    }

    public Object choose1() {
      final List found = new LinkedList();
      foreach(new Predicate(){
        public boolean eval(Object x) {
          found.add(x);
          return false;
        }
      });
      return found.get(0);
    }

    public String toString() {
      return toString(PATIENCE);
    }

    public String toString(final int patience) {
      if (id != null) return id;

      final SortedSet<String> content =
          new TreeSet<String>(new Comparator<String>() {
            public int compare(String a, String b) {
              return
                  b.equals(a) ? 0 :
                  a.length() < b.length() ? -1 :
                  a.length() > b.length() ? 1 :
                  a.compareTo(b);
            }
          });
      final int[] size = {0};
      foreach(new Predicate() {
        public boolean eval(Object x) {
          int left = patience - size[0];
          String s = x instanceof SetZ ? ((SetZ)x).toString(left) : x.toString();
          size[0] += s.length();
          content.add(s);
          return left > 0;
        }
      });
      StringBuilder sb = new StringBuilder("{");
      for (String s : content) {
        if (sb.length() > 1) sb.append(",");
        sb.append(s);
        if (sb.length() > patience) {
          sb.append("...");
          break;
        }
      }
      sb.append("}");
      return sb.toString();
    }
  }

  // Axiom II (empty set)
  public final SetZ EMPTY = exists(new SetZ() {
    public boolean contains(Object x) {
      return false;
    }
  });

  // Axiom III (separation)
  public SetZ separation(final Predicate p, final SetZ s) {
    return exists(new SetZ() {
      public boolean contains(Object x) {
        return p.eval(x) && s.contains(x);
      }
    });
  }

  // Axiom IV (powerset)
  public SetZ powerset(final SetZ s) {
    return exists(new SetZ() {
      public boolean contains(Object x) {
        return x instanceof SetZ && ((SetZ)x).isSubsetOf(s);
      }
    });
  }

  // Axiom V (union)
  public SetZ union(final SetZ s) {
    return exists(new SetZ() {
      public boolean contains(final Object x) {
        return x instanceof SetZ && exists(new Predicate() {
          public boolean eval(Object y) {
            return s.contains(y) && y instanceof SetZ && ((SetZ)y).contains(x);
          }
        });
      }
    });
  }

  // Axiom VI choice
  public SetZ choice (final SetZ s) {
    return exists(new SetZ() {
      public boolean contains(final Object x) {
        return exists(new Predicate() { // existence
          public boolean eval(final Object y) {
            return s.contains(y) && x == ((SetZ)y).choose1();
          }
        });
      }
    });
  }

  class Singleton extends SetZ {
    Object x;

    public Singleton(Object x) {
      this.x = x;
    }

    public Object element() {
      return x;
    }

    public boolean contains(Object y) {
      return x == y;
    }

    public boolean equals(Object other) {
      return other == this ||
          other instanceof Singleton && x == ((Singleton)other).x;
    }

    public boolean isSubsetOf(SetZ s) {
      return s.contains(x);
    }

    public String toString() {
      return "{" + x + "}";
    }

    public String toString(int patience) {
      return id != null ? id : ("{" + (x instanceof SetZ ? ((SetZ)x).toString(patience) : x.toString()) + "}");
    }
  }

  // Axiom VII infinity
  public SetZ INFINITY = exists(new SetZ() {
    public boolean contains(Object x) {
      boolean isaMember =
          x == EMPTY ||
         (x instanceof Singleton && contains(((Singleton)x).element()));
      if (isaMember) existsSingletonFor(x);
      return isaMember;
    }
  });

  public SetZ withElements(final Collection source) {
    return exists(new SetZ() {
      public boolean contains(Object x) {
        return source.contains(x);
      }
    });
  }

  public SetZ withElements(final Object... elements) {
    return withElements(new HashSet(Arrays.asList(elements)));
  }

  public static void main(String[] args) {
    Zermelo setTheory = new Zermelo() {
      public void log(String s) {
        System.out.println(s);
      }
    };
    System.out.println("Set Theory Created. (Thank you, Leopold and Thoralf!)");
    setTheory.EMPTY.denote("0"/*"∅"*/);
    setTheory.INFINITY.denote("N"/*"ℕ"*/);
    final SetZ powerofAleph0 = setTheory.powerset(setTheory.INFINITY).denote("R"/*"ℝ"*/);
    final SetZ a = setTheory.existsSingletonFor(setTheory.existsSingletonFor('a')).denote("A");
    final SetZ b = setTheory.existsSingletonFor(setTheory.existsSingletonFor('b')).denote("B");
    final SetZ aORb = setTheory.exists(setTheory.new SetZ(){
      public boolean contains(Object x) {
        return x == a || x == b;
      }
    }).denote("AorB");

    final SetZ twoSets = setTheory.exists(setTheory.new SetZ() {
      public boolean contains(Object x) {
        return x == aORb || x == powerofAleph0;
      }
    });
    SetZ choiceOf2 = setTheory.choice(twoSets);
    System.out.println("AC for " + twoSets + "? " + choiceOf2);
  }
}
