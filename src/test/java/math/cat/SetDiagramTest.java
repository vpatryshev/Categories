package math.cat;

import static java.math.cat.Base.List;
import static java.math.cat.Base.Map;
import static java.math.cat.Base.array;
import static java.math.cat.BasePair.Pair;
import static java.math.cat.Category.Category;
import static java.math.cat.Sets.Set;

import java.math.cat.*;
import java.math.cat.Functions;
import java.math.cat.Functions.Function;

import java.math.cat.Functor;
import java.math.cat.SetMorphism;
import java.math.cat.Sets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

/**
 * Unittest for SetDiagram class
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 */
@SuppressWarnings("unchecked")
public class SetDiagramTest extends TestCase {

  public void testConstructor() {
    Set[] codomainObjects = buildObjects("12", "12", "12", "12");
    buildDiagram(Categories.SQUARE, codomainObjects);
    // validator will tell us if it's good or not.
  }

  public void testLimit_productActually() {
    SetDiagram<String, String> diagram = buildDiagram(Categories._1plus1_, buildObjects("12", "34"));
    java.math.cat.Functor<String, String, Set, java.math.cat.TypelessSetMorphism>.Cone cone = diagram.limit();
    Set actual = cone.apex();
    Set expected = java.math.cat.Sets.Set(List("b3", "a1"), List("b4", "a1"), List("b3", "a2"), List("b4", "a2"));
    assertEquals(expected, actual);
  }

  public void testLimit_pullbackActually() {
    SetDiagram<String, String> diagram = buildDiagram(Categories.PULLBACK, buildObjects("123", "234", "012345"));
    java.math.cat.Functor<String, String, Set, TypelessSetMorphism>.Cone cone = diagram.limit();
    Set actual = cone.apex();
    Set expected = java.math.cat.Sets.Set(List("b2", "a2"), List("b3", "a3"));
    assertEquals(expected, actual);
  }

  public void testLimit_W() {
    SetDiagram<String, String> diagram = buildDiagram(Categories.W, buildObjects("123", "012345", "234", "23456", "345"));
    java.math.cat.Functor<String, String, Set, java.math.cat.TypelessSetMorphism>.Cone cone = diagram.limit();
    Set actual = cone.apex();
    Set expected = Collections.singleton(List("e3", "c3", "a3"));
    assertEquals(expected, actual);
  }

  public void testLimit_M() {
    SetDiagram<String, String> diagram = buildDiagram(Categories.M, buildObjects("01234567", "123", "012345", "234", "23456"));
    java.math.cat.Functor<String, String, Set, java.math.cat.TypelessSetMorphism>.Cone cone = diagram.limit();
    Set actual = cone.apex();
    Set expected = java.math.cat.Sets.Set(List("d2", "b2"), List("d3", "b3"));
    assertEquals(expected, actual);
  }

  public void testLimit_coequalizerDiagram() {
    final Category<String, String> COEQUALIZER = Category("(([a,b,c,d], {ac: a -> c, bc: b -> c, cd: c -> d, ad: a -> d, bd: b -> d}), {cd o ac = ad, cd o bc = bd})");
    SetDiagram<String, String> diagram = buildDiagram(COEQUALIZER, buildObjects("123", "2345", "023", "0"));
    java.math.cat.Functor<String, String, Set, TypelessSetMorphism>.Cone actual = diagram.limit();
    Set apex = actual.apex();
    Set expected = java.math.cat.Sets.Set(List("b4", "a1"), List("b5", "a1"), List("b2", "a2"), List("b3", "a3"));
    assertEquals(expected, apex);
  }
  
  public void testLimit_Z2() {
    SetDiagram<String, String> diagram = buildDiagram(Categories.Z2, buildObjects("123"));
    java.math.cat.Functor<String, String, Set, java.math.cat.TypelessSetMorphism>.Cone actual = diagram.limit();
    Set apex = actual.apex();
    assertEquals(Sets.Set(List("a1"), List("a2"), List("a3")), apex);
  }

  public void testLimit_SplitMono() {
    SetDiagram<String, String> diagram = 
          buildDiagram(
              Categories.SPLIT_MONO, 
              buildObjects("01", "0123"),
              Map(array("bb"), array(Map(array("b0", "b1", "b2", "b3"), array("b0", "b1", "b0", "b0")))));
    java.math.cat.Functor<String, String, Set, TypelessSetMorphism>.Cone actual = diagram.limit();
    assertEquals(java.math.cat.Sets.Set(List("b0", "a0"), List("b1", "a1")), actual.apex());
  }
  
  public void testColimit_unionActually() {
    SetDiagram<String, String> diagram = buildDiagram(Categories._1plus1_, buildObjects("12", "34"));
    java.math.cat.Functor<String, String, Set, java.math.cat.TypelessSetMorphism>.Cocone cocone = diagram.colimit();
    Set actual = cocone.apex();
    Set expected = Set(Set(Pair(0, "b3")), Set(Pair(0, "b4")), Set(Pair(1, "a1")), Set(Pair(1, "a2")));
    assertEquals(expected, actual);
  }

  public void testColimit_pushoutActually() {
    SetDiagram<String, String> diagram = 
        buildDiagram(Categories.PUSHOUT, buildObjects("123", "12345", "01234"));
    java.math.cat.Functor<String, String, Set, java.math.cat.TypelessSetMorphism>.Cocone actual = diagram.colimit();
    Set actualSet = actual.apex();
    Set expectedSet = Set(
        Set(Pair(1, "c0")), 
        Set(Pair(0, "b1"), Pair(1, "c1")), 
        Set(Pair(0, "b2"), Pair(1, "c2")), 
        Set(Pair(0, "b3"), Pair(1, "c3")), 
        Set(Pair(1, "c4")), 
        Set(Pair(0, "b4")), 
        Set(Pair(0, "b5")));
    assertEquals(expectedSet, actualSet);
    assertEquals(
        Map(
            array("b1", "b2", "b3", "b4", "b5"), 
            array(Set(Pair(0, "b1"), Pair(1, "c1")), Set(Pair(0, "b2"), Pair(1, "c2")), Set(Pair(0, "b3"), Pair(1, "c3")), Set(Pair(0, "b4")), Set(Pair(0, "b5")))), 
        actual.arrowFrom("b").asMap());
  }

  public void testColimit_W() {
    SetDiagram<String, String> diagram = buildDiagram(Categories.W, buildObjects("123", "012345", "234", "23456", "345"));
    Functor<String, String, Set, java.math.cat.TypelessSetMorphism>.Cocone cocone = diagram.colimit();
    Set actual = cocone.apex();
    Set expected = Set(
        Set(Pair(1, "b0")), 
        Set(Pair(1, "b1")), 
        Set(Pair(1, "b2"), Pair(0, "d2")), 
        Set(Pair(1, "b3"), Pair(0, "d3")), 
        Set(Pair(1, "b4"), Pair(0, "d4")), 
        Set(Pair(1, "b5")), Set(Pair(0, "d5")), 
        Set(Pair(0, "d6")));
    assertEquals(expected, actual);
  }

  public void testColimit_M() {
    SetDiagram<String, String> diagram = buildDiagram(Categories.M, buildObjects("01234567", "123", "012345", "234", "23456"));
    java.math.cat.Functor<String, String, Set, java.math.cat.TypelessSetMorphism>.Cocone cocone = diagram.colimit();
    Set actual = cocone.apex();
    Set expected = Set(
        Set(Pair(2, "a0")), Set(Pair(1, "c0")), 
        Set(Pair(2, "a1"), Pair(1, "c1")), 
        Set(Pair(2, "a2"), Pair(1, "c2"), Pair(0, "e2")), 
        Set(Pair(2, "a3"), Pair(1, "c3"), Pair(0, "e3")), 
        Set(Pair(2, "a4")), Set(Pair(1, "c4"), Pair(0, "e4")), 
        Set(Pair(2, "a5")), Set(Pair(1, "c5")), Set(Pair(0, "e5")),
        Set(Pair(2, "a6")), Set(Pair(0, "e6")),
        Set(Pair(2, "a7")));
    assertEquals(expected, actual);
  }

  private Set<String> buildTestSet(String prefix, String values) {
    Set<String> set = new HashSet<String>();
    for (int i = 0; i < values.length(); i++) {
      set.add(prefix + values.charAt(i));
    }
    return set;
  }

  private Set[] buildObjects(String... content) {
    List<Set> codomainObjects = new ArrayList<Set>();
    char c = 'a';
    for (String values : content) {
      codomainObjects.add(buildTestSet("" + c, values));
      c++;
    }
    return codomainObjects.toArray(new Set[content.length]);
  }
  
  private SetDiagram<String, String> buildDiagram(final Category<String, String> domain, Set[] codomainObjects) {
    return buildDiagram(domain, codomainObjects, new HashMap<String, Map<String, String>>());
  }

  private SetDiagram<String, String> buildDiagram(final Category<String, String> domain, Set[] codomainObjects, final Map<String, Map<String, String>> customMapping) {
    Category<Set, java.math.cat.TypelessSetMorphism> codomain = Categories.SETF;
    String[] domainObjects = domain.objects().toArray(new String[domain.objects().size()]);
    Arrays.sort(domainObjects);
    final Map<String, Set> objectMapper = java.math.cat.Base.Map(domainObjects, codomainObjects);
    java.math.cat.SetMorphism<String, Set<String>, Set, Set<Set>> om =
        java.math.cat.SetMorphism.Morphism(domain.objects(), codomain.objects(), objectMapper);

    Function<String, java.math.cat.TypelessSetMorphism> arrowMapper =
        new Function<String, java.math.cat.TypelessSetMorphism>() {
          @Override
          public java.math.cat.TypelessSetMorphism apply(String s) {
            final String domainName = domain.d0(s);
            final Set  arrowDomain = objectMapper.get(domainName);
            final String codomainName = domain.d1(s);
            final Set arrowCodomain = objectMapper.get(codomainName);
            assertNotNull("Domain with name '" + domainName + "' should not be null for string " + s, arrowDomain);
            assertNotNull("Codomain with name '" + codomainName + "' should not be null for string " + s, arrowCodomain);
            return customMapping.containsKey(s) ? 
                java.math.cat.TypelessSetMorphism.forFunction(arrowDomain, arrowCodomain, Functions.forMap(customMapping.get(s))) :
                s.length() == 1 ?
                java.math.cat.TypelessSetMorphism.inclusion(arrowDomain, arrowCodomain) :
                new java.math.cat.TypelessSetMorphism(arrowDomain, arrowCodomain) {

                  @Override
                  public Object apply(Object o) {
                    String candidate = codomainName + o.toString().substring(1);
                    Set codomainSet = objectMapper.get(codomainName);
                    return codomainSet.contains(candidate) ? candidate : (codomainName + "0");
                  }
                };
          }
        };
    java.math.cat.SetMorphism<String, Set<String>, TypelessSetMorphism, Set<TypelessSetMorphism>> am =
        SetMorphism.Morphism(domain.arrows(), codomain.arrows(), arrowMapper);
    return new SetDiagram<String, String>(domain, om, am);
  }
  
  public void testColimit_Z2() {
    SetDiagram<String, String> diagram = buildDiagram(Categories.Z2, buildObjects("123"));
    java.math.cat.Functor<String, String, Set, TypelessSetMorphism>.Cocone actual = diagram.colimit();
    Set apex = actual.apex();
    assertEquals(java.math.cat.Sets.Set(Set(Pair(0, "a1")), Set(Pair(0, "a2")), Set(Pair(0, "a3"))), apex);
  }

  public void testColimit_SplitMono() {
    SetDiagram<String, String> diagram = 
          buildDiagram(
              Categories.SPLIT_MONO,
              buildObjects("01", "0123"),
              Map(array("bb"), array(Map(array("b0", "b1", "b2", "b3"), array("b0", "b1", "b0", "b0")))));
    java.math.cat.Functor<String, String, Set, TypelessSetMorphism>.Cocone actual = diagram.colimit();
    assertEquals(java.math.cat.Sets.Set(Set(Pair(0, "b1"), Pair(1, "a1")), Set(Pair(0, "b0"), Pair(0, "b2"), Pair(0, "b3"), Pair(1, "a0"))), actual.apex());
  }
}