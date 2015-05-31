package math.cat;

import java.util.Set;

/**
 * Abstraction of morphism (aka arrow), from categorical point of view.
 * two essential methods are defined here, domain() and codomain(). There is no way
 * to define or declare composition: what is the type of the result? How can it be instantiated?
 *
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 */
public abstract class Morphism<X, Y> {
  private String name;
  private X domain;
  private Y codomain;

  /**
   * Base constructor. Takes domain and codomain
   *
   * @param domain domain of this morphism
   * @param codomain codomain of this morphism
   */
  Morphism(X domain, Y codomain) {
    this.domain = domain;
    this.codomain = codomain;
  }

  /**
   * Constructor.
   *
   * @param name name of this morphism
   * @param domain domain of this morphism
   * @param codomain codomain of this morphism
   */
  Morphism(String name, X domain, Y codomain) {
    this(domain, codomain);
    this.name = name;
  }

  /**
   * @return name of this morphism
   */
  public String name() { return name; }

  /**
   * @return domain of this morphism
   */
  public X domain() { return domain; }

  /**
   * @return codomain of this morphism
   */
  public Y codomain() { return codomain; }

  /**
   * @return String representation of this morphism
   */
  @Override public String toString() { return name == null ? super.toString() : name; }
}
