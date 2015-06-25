package j.math.cat;

import static j.math.cat.BasePair.Pair;
import static j.math.cat.Category.Category;
import static j.math.cat.Sets.*;
import j.math.cat.Functor;

import j.math.cat.Functions.Function;

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
    Functor<String, String, Set, TypelessSetMorphism>.Cone cone = diagram.limit();
    Set actual = cone.apex();
    Set expected = Sets.Set(Base.List("b3", "a1"), Base.List("b4", "a1"), Base.List("b3", "a2"), Base.List("b4", "a2"));
    assertEquals(expected, actual);
  }

  public void testLimit_pullbackActually() {
    SetDiagram<String, String> diagram = buildDiagram(Categories.PULLBACK, buildObjects("123", "234", "012345"));
    Functor<String, String, Set, TypelessSetMorphism>.Cone cone = diagram.limit();
    Set actual = cone.apex();
    Set expected = Sets.Set(Base.List("b2", "a2"), Base.List("b3", "a3"));
    assertEquals(expected, actual);
  }

  public void testLimit_W() {
    SetDiagram<String, String> diagram = buildDiagram(Categories.W, buildObjects("123", "012345", "234", "23456", "345"));
    Functor<String, String, Set, TypelessSetMorphism>.Cone cone = diagram.limit();
    Set actual = cone.apex();
    Set expected = Collections.singleton(Base.List("e3", "c3", "a3"));
    assertEquals(expected, actual);
  }

  public void testLimit_M() {
    SetDiagram<String, String> diagram = buildDiagram(Categories.M, buildObjects("01234567", "123", "012345", "234", "23456"));
    Functor<String, String, Set, TypelessSetMorphism>.Cone cone = diagram.limit();
    Set actual = cone.apex();
    Set expected = Sets.Set(Base.List("d2", "b2"), Base.List("d3", "b3"));
    assertEquals(expected, actual);
  }

  public void testLimit_coequalizerDiagram() {
    final Category<String, String> COEQUALIZER = Category("(([a,b,c,d], {ac: a -> c, bc: b -> c, cd: c -> d, ad: a -> d, bd: b -> d}), {cd o ac = ad, cd o bc = bd})");
    SetDiagram<String, String> diagram = buildDiagram(COEQUALIZER, buildObjects("123", "2345", "023", "0"));
    Functor<String, String, Set, TypelessSetMorphism>.Cone actual = diagram.limit();
    Set apex = actual.apex();
    Set expected = Sets.Set(Base.List("b4", "a1"), Base.List("b5", "a1"), Base.List("b2", "a2"), Base.List("b3", "a3"));
    assertEquals(expected, apex);
  }

    private SetDiagram<String, String> Z2Diagram = buildDiagram(Categories.Z2, buildObjects("123"),
            new HashMap<String, Map<String, String>>() {{
                Map<String, String> map = new HashMap<String, String>();
                map.put("a1", "a1"); map.put("a2", "a3"); map.put("a3", "a2");
                put("-1", map);
            }});

  public void testLimit_Z2() {
      SetDiagram<String, String>.Limit limitData = Z2Diagram.new Limit();

      final List<String> a1s = Base.List("a1");
      Set expectedApex = Sets.Set(Base.List(a1s));

    Functor<String, String, Set, TypelessSetMorphism>.Cone actual = Z2Diagram.limit();
    Set apex = actual.apex();

    assertEquals("Could not calculate limit on Z2: " + actual + "\nfrom " +
            limitData.listOfObjects + ", " + limitData.participantArrows + " with apex " + limitData.apex + "\nthe diagram was " + Z2Diagram,
            expectedApex, apex);
  }

  public void testLimit_SplitMono() {
    SetDiagram<String, String> diagram =
          buildDiagram(
              Categories.SPLIT_MONO,
              buildObjects("01", "0123"),
              Base.Map(Base.array("bb"), Base.array(Base.Map(Base.array("b0", "b1", "b2", "b3"), Base.array("b0", "b1", "b0", "b0")))));
    Functor.Cone actual = diagram.limit();
    TestCase.assertEquals(Sets.Set(Base.List("b0", "a0"), Base.List("b1", "a1")), actual.apex());
  }

  public void testColimit_unionActually() {
    SetDiagram<String, String> diagram = buildDiagram(Categories._1plus1_, buildObjects("12", "34"));
    Functor<String, String, Set, TypelessSetMorphism>.Cocone cocone = diagram.colimit();
    Set actual = cocone.apex();
    Set expected = Sets.Set(Sets.Set(BasePair.Pair(0, "b3")), Sets.Set(BasePair.Pair(0, "b4")), Sets.Set(BasePair.Pair(1, "a1")), Sets.Set(BasePair.Pair(1, "a2")));
    assertEquals(expected, actual);
  }

  public void testColimit_pushoutActually() {
    SetDiagram<String, String> diagram =
        buildDiagram(Categories.PUSHOUT, buildObjects("123", "12345", "01234"));
    Functor<String, String, Set, TypelessSetMorphism>.Cocone actual = diagram.colimit();
    Set actualSet = actual.apex();
    Set expectedSet = Sets.Set(
            Sets.Set(BasePair.Pair(1, "c0")),
            Sets.Set(BasePair.Pair(0, "b1"), BasePair.Pair(1, "c1")),
            Sets.Set(BasePair.Pair(0, "b2"), BasePair.Pair(1, "c2")),
            Sets.Set(BasePair.Pair(0, "b3"), BasePair.Pair(1, "c3")),
            Sets.Set(BasePair.Pair(1, "c4")),
            Sets.Set(BasePair.Pair(0, "b4")),
            Sets.Set(BasePair.Pair(0, "b5")));
    assertEquals(expectedSet, actualSet);

      final Map<String, Set<Pair<Integer, String>>> expected = Base.Map(
              Base.array("b1", "b2", "b3", "b4", "b5"),
              Base.array(Sets.Set(BasePair.Pair(0, "b1"), BasePair.Pair(1, "c1")), Sets.Set(BasePair.Pair(0, "b2"), BasePair.Pair(1, "c2")), Sets.Set(BasePair.Pair(0, "b3"), BasePair.Pair(1, "c3")), Sets.Set(BasePair.Pair(0, "b4")), Sets.Set(BasePair.Pair(0, "b5"))));

      final TypelessSetMorphism arrow = actual.arrowFrom("b");
      assertNotNull("Arrow from b should not be null", arrow);
      final Map<Object, Object> actual1 = arrow.asMap();

      assertEquals(expected, actual1);
  }

  public void testColimit_W() {
    SetDiagram<String, String> diagram = buildDiagram(Categories.W, buildObjects("123", "012345", "234", "23456", "345"));
    Functor<String, String, Set, TypelessSetMorphism>.Cocone cocone = diagram.colimit();
    Set actual = cocone.apex();
    Set expected = Sets.Set(
            Sets.Set(BasePair.Pair(1, "b0")),
            Sets.Set(BasePair.Pair(1, "b1")),
            Sets.Set(BasePair.Pair(1, "b2"), BasePair.Pair(0, "d2")),
            Sets.Set(BasePair.Pair(1, "b3"), BasePair.Pair(0, "d3")),
            Sets.Set(BasePair.Pair(1, "b4"), BasePair.Pair(0, "d4")),
            Sets.Set(BasePair.Pair(1, "b5")), Sets.Set(BasePair.Pair(0, "d5")),
            Sets.Set(BasePair.Pair(0, "d6")));
    assertEquals(expected, actual);
  }

  public void testColimit_M() {
    SetDiagram<String, String> diagram = buildDiagram(Categories.M, buildObjects("01234567", "123", "012345", "234",  "23456"));
    Functor<String, String, Set, TypelessSetMorphism>.Cocone cocone = diagram.colimit();
    Set actual = cocone.apex();
    Set expected = Sets.Set(
            Sets.Set(BasePair.Pair(2, "a0")), Sets.Set(BasePair.Pair(1, "c0")),
            Sets.Set(BasePair.Pair(2, "a1"), BasePair.Pair(1, "c1")),
            Sets.Set(BasePair.Pair(2, "a2"), BasePair.Pair(1, "c2"), BasePair.Pair(0, "e2")),
            Sets.Set(BasePair.Pair(2, "a3"), BasePair.Pair(1, "c3"), BasePair.Pair(0, "e3")),
            Sets.Set(BasePair.Pair(2, "a4")), Sets.Set(BasePair.Pair(1, "c4"), BasePair.Pair(0, "e4")),
            Sets.Set(BasePair.Pair(2, "a5")), Sets.Set(BasePair.Pair(1, "c5")), Sets.Set(BasePair.Pair(0, "e5")),
            Sets.Set(BasePair.Pair(2, "a6")), Sets.Set(BasePair.Pair(0, "e6")),
            Sets.Set(BasePair.Pair(2, "a7")));
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
      domain.validate();
    Category<Set, TypelessSetMorphism> codomain = Categories.SETF;
    String[] domainObjects = domain.objects().toArray(new String[domain.objects().size()]);
    Arrays.sort(domainObjects);
    final Map<String, Set> objectMapper = Base.Map(domainObjects, codomainObjects);
    SetMorphism<String, Set<String>, Set, Set<Set>> om =
        SetMorphism.Morphism(domain.objects(), codomain.objects(), objectMapper);

    Function<String, TypelessSetMorphism> arrowMapper =
        new Function<String, TypelessSetMorphism>() {
          @Override
          public TypelessSetMorphism apply(final String s) {
            final String domainName = domain.d0(s);
            final Set  arrowDomain = objectMapper.get(domainName);
            final String codomainName = domain.d1(s);
            final Set arrowCodomain = objectMapper.get(codomainName);
            assertNotNull("Domain with name '" + domainName + "' should not be null for string " + s, arrowDomain);
            assertNotNull("Codomain with name '" + codomainName + "' should not be null for string " + s, arrowCodomain);
            return customMapping.containsKey(s) ?
                TypelessSetMorphism.forFunction(arrowDomain, arrowCodomain, Functions.forMap(customMapping.get(s))) :
                s.length() == 1 ?
                TypelessSetMorphism.inclusion(arrowDomain, arrowCodomain) :
                new TypelessSetMorphism(arrowDomain, arrowCodomain) {
                    private String name = s;
                  @Override
                  public Object apply(Object o) {
                    String candidate = codomainName + o.toString().substring(1);
                    Set codomainSet = objectMapper.get(codomainName);
                    return codomainSet.contains(candidate) ? candidate : (codomainName + "0");
                  }
                };
          }
        };

    SetMorphism<String, Set<String>, TypelessSetMorphism, Set<TypelessSetMorphism>> am =
        SetMorphism.Morphism(domain.arrows(), codomain.arrows(), arrowMapper);

    return new SetDiagram<String, String>(domain, om, am);
  }

  public void testColimit_Z2() {
    Functor<String, String, Set, TypelessSetMorphism>.Cocone actual = Z2Diagram.colimit();
    Set apex = actual.apex();
    TestCase.assertEquals(Sets.Set(Sets.Set(BasePair.Pair(0, "a2"), BasePair.Pair(0, "a3")), Sets.Set(BasePair.Pair(0, "a1"))), apex);
  }

  public void testColimit_SplitMono() {
    SetDiagram<String, String> diagram =
          buildDiagram(
              Categories.SPLIT_MONO,
              buildObjects("01", "0123"),
              Base.Map(Base.array("bb"), Base.array(Base.Map(Base.array("b0", "b1", "b2", "b3"), Base.array("b0", "b1", "b0", "b0")))));
    Functor.Cocone actual = diagram.colimit();
    TestCase.assertEquals(Sets.Set(Sets.Set(BasePair.Pair(0, "b1"), BasePair.Pair(1, "a1")), Sets.Set(BasePair.Pair(0, "b0"), BasePair.Pair(0, "b2"), BasePair.Pair(0, "b3"), BasePair.Pair(1, "a0"))), actual.apex());
  }
}