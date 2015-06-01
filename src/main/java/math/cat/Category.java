package math.cat.v0;

import java.util.Set;

/**
 * Sample Implementation of category.
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 */
public abstract class Category<O, A> extends Graph<O, A> {

  public Set<O> objects() { return nodes(); }
  public abstract A unit(O x);
  public abstract A m(A f, A g);

  /*
   * Java technicalities: have to override these methods.
   */
  @Override public boolean equals(Object o) { return o instanceof Category && equals((Category)o); }

  private Category(Set<O> objects) {
    super(objects);
    validate();
  }

  /**
   * Validates this category, checking all the axioms.
   */
  public void validate() {
    super.validate();

    for (O x : objects()) {
      A unit = unit(x);
      assert arrows().contains(unit) : "Unit for " + x + " not defined";
      assert d0(unit).equals(x) : "Domain of unit " + unit + " should be " + x;
      assert d1(unit).equals(x) : "Codomain of unit " + unit + " should be " + x;
    }

    for (A f : arrows()) {
      assert m(unit(d0(f)), f).equals(f) : "Left unit law broken for " + unit(d0(f)) + " and " + f;
      assert m(f, unit(d1(f))).equals(f) : "Right unit law broken for " + unit(d0(f)) + " and " + f;
    }

    for (A f : arrows()) for (A g : arrows()) if (d1(f).equals(d0(g))) {
      A fg = m(f, g);
      assert fg != null : "Composition of " + f + " and " + g + " not defined";
      assert d0(fg).equals(d0(f)): "Wrong composition " + fg + " of " + f + " and " + g + ": its d0 is " + d0(fg) + ", must be " + d0(f);
      assert d1(fg).equals(d1(g)): "Wrong composition " + fg + " of " + f + " and " + g + ": its d1 is " + d1(fg) + ", must be " + d1(g);
    }

    for (A f : arrows()) for (A g : arrows()) for (A h : arrows()) if (d1(f).equals(d0(g)) && d1(g).equals(d0(h))) {
      assert m(m(f, g), h).equals(m(f, m(g, h))) : "Associativity broken for " + f + ", " + g + " and " + h;
    }
  }

  private boolean equals(Category<O, A> other) {
    boolean isEqual = // two categories are equal if:
        objects().equals(other.objects()) && // they have the same objects
        arrows().equals(other.arrows()); // and they have the same arrows

    for (O x : objects()) {
      isEqual = isEqual && (unit(x).equals(other.unit(x))); // and object have the same unit arrows
    }

    for (A f : arrows()) {
      isEqual = isEqual &&
          (d0(f).equals(other.d0(f))) && // and arrows have the same domains
          (d1(f).equals(other.d1(f)));   // and the same codomains
      for (A g : arrows()) if (d1(f).equals(d0(g))) {
        isEqual = isEqual && m(f, g).equals(other.m(f, g)); // and arrow composition is the same
      }
    }
    return isEqual;
  }
}