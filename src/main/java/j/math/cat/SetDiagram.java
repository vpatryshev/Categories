package j.math.cat;

import static j.math.cat.BasePair.Pair;

import j.math.cat.Functions.Function;
import j.math.cat.Functions.Id;
import j.math.cat.Functions.Injection;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Diagram from a category to Categories.SETF.
 * @param <Objects> type of objects of domain category
 * @param <Arrows> type of arrows of domain category
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 */
@SuppressWarnings("unchecked")
public class SetDiagram<Objects, Arrows>
    extends Functor<Objects, Arrows, Set, TypelessSetMorphism> {

  /**
   * Constructor. Builds unnamed functor.
   *
   * @param domain          domain category
   * @param objectsMorphism maps objects from the first category to the objects of the second category
   * @param arrowsMorphism  maps arrows from the first category to the arrows of the second category
   */
  public SetDiagram(
      Category<Objects, Arrows> domain,
      SetMorphism<Objects, Set<Objects>, Set, Set<Set>> objectsMorphism,
      SetMorphism<Arrows, Set<Arrows>, TypelessSetMorphism, Set<TypelessSetMorphism>> arrowsMorphism) {
    super(domain, Categories.SETF, objectsMorphism, arrowsMorphism);
    validate();
  }

  @Override
  public Cone limit() {
    Set<Objects> participantObjects = domain().allInitialObjects();
    Set<Arrows> participantArrows = domain().arrowsFromInitialObjects();

    final List<Objects> listOfObjects = new ArrayList<Objects>(participantObjects);

    Function<Objects, Integer> index = new Function<Objects, Integer>() {
      @Override
      public Integer apply(Objects x) {
        return listOfObjects.indexOf(x);
      }
    };

    // this function takes an object and returns a projection set function; we have to compose each such projection
    // with the right arrow from important object to the image of our object
    final Function<Objects, Function<List<Object>, Object>> projectionForObject = index.then(Sets.Cartesian.projection());
    
    final Set<? extends List<Object>> apex = calculateLimitApex(participantArrows, listOfObjects, projectionForObject);

    // now have the apex; build the cone
    final Map<Objects, Set<Arrows>> bundles =
      domain().buildBundles(participantObjects, participantArrows);

    // For each object of domain we have an arrow from one of the objects used in building the product
    Map<Objects, Arrows> arrowFromImportantObject = new HashMap<Objects, Arrows>();
    for (Objects object : bundles.keySet()) {
      arrowFromImportantObject.put(object, domain().unit(object));
      for (Arrows arrow : bundles.get(object)) {
        arrowFromImportantObject.put(domain().d1(arrow), arrow);
      }
    }
    Function<Objects, Arrows> importantArrowForObject = Functions.forMap(arrowFromImportantObject);

    // takes an object, returns an arrow from an important object to the image of object in SETF
    final Function<Objects, TypelessSetMorphism> componentMorphismForObject = Functions.compose(importantArrowForObject,
            this.arrowsMorphism.asFunction());

    Map<Objects, TypelessSetMorphism> coneMap = new Function<Objects, TypelessSetMorphism>() {

      @Override
      public TypelessSetMorphism apply(Objects x) {
        TypelessSetMorphism componentMorphism = componentMorphismForObject.apply(x);
        return TypelessSetMorphism.forFunction(apex, componentMorphism.codomain(),
            projectionForObject.apply(x).then(componentMorphism.asFunction()));
      }
    }.toMap(domain().objects());

    return new Cone(apex, coneMap);
  }

  private Set<? extends List<Object>> calculateLimitApex(Set<Arrows> participantArrows,
          final List<Objects> listOfObjects, final Function<Objects, Function<List<Object>, Object>> projectionForObject) {
    // Here we have a non-repeating collection of sets to use for building a product
    List<Set> setsToUse = nodesMorphism.asFunction().map(listOfObjects);
    Set<? extends List<Object>> product = Sets.Cartesian.product(setsToUse);
    final Map<Objects, Set<Arrows>> cobundles =
      domain().op().buildBundles(domain().objects(), participantArrows);

    // Have a product set; have to remove all the bad elements from it
    Predicate<List<Object>> compatibleListsOnly = new Predicate<List<Object>>() {
      @Override
      public boolean eval(final List<Object> point) {
        Predicate<Set<Arrows>> checkCompatibility =
            allArrowsAreCompatibleOnPoint(projectionForObject, point);
        return checkCompatibility.forall(cobundles.values());
      }
    };
    return compatibleListsOnly.filter(product);
  }

  /**
   * Builds a predicate that checks if a given set of arrows map a given element of Cartesian product to the same value
   * @param projectionForObject given an object, returns a function that maps a point to its component in the object image
   * @param point element of Cartesian product
   * @return the predicate
   */
  Predicate<Set<Arrows>> allArrowsAreCompatibleOnPoint(final Function<Objects, Function<List<Object>, Object>> projectionForObject, final List<Object> point) {
    return new Predicate<Set<Arrows>>() {
      @Override
      public boolean eval(final Set<Arrows> setOfArrows) {
        return arrowsAreCompatibleOnPoint(projectionForObject, point).forall(Sets.product(setOfArrows, setOfArrows));
      }
    };
  }

  /**
   * Builds a predicate that checks whether two arrows's action on a given point produce the same element. 
   * @param projectionForObject given an object, returns a function that maps a point to its component in the object image
   * @param point an element of Cartesian product
   * @return the predicate
   */
  BinaryRelationship<Arrows, Arrows> arrowsAreCompatibleOnPoint(
          final Function<Objects, Function<List<Object>, Object>> projectionForObject, 
          final List<Object> point) {
    return new BinaryRelationship<Arrows, Arrows>() {
      @Override
      public boolean eval(Pair<Arrows, Arrows> pair) {
        Object fx = arrowActsOnAPoint(pair.x(), projectionForObject, point);
        Object fy = arrowActsOnAPoint(pair.y(), projectionForObject, point);
        return Category.equal(fx, fy);
      }
    };
  }

  /**
   * Calculates the action of a given arrow on an element of Cartesian product (which is called 'point').
   * 
   * @param a the arrow
   * @param projectionForObject given an object, returns a function that maps a point to its component in the object image
   * @param point a list of elements in the sets corresponding to objects from the list above
   * @return the object to which the arrow maps a component of the list
   */
  Object arrowActsOnAPoint(Arrows a, 
          final Function<Objects, Function<List<Object>, Object>> projectionForObject, 
          List<Object> point) {
    TypelessSetMorphism f = arrowsMorphism.apply(a);
    return f.apply(projectionForObject.apply(domain().d0(a)).apply(point));
  }

  @Override
  public Functor<Objects, Arrows, Set, TypelessSetMorphism>.Cocone colimit() {
    Set<Objects> participantObjects = domain().op().allInitialObjects();
    Set<Arrows> participantArrows = domain().op().arrowsFromInitialObjects();
    final Map<Objects, Set<Arrows>> bundles =
        domain().buildBundles(domain().objects(), participantArrows);

    final List<Objects> listOfObjects = new ArrayList<Objects>(participantObjects);
    // Here we have a non-repeating collection of sets to use for building a union
    List<Set> setsToUse = nodesMorphism.asFunction().map(listOfObjects);
    List<Set<Object>> setsToJoin = new Id().map(setsToUse);
    Sets.DisjointUnion<Object> du = new Sets.DisjointUnion<Object>(setsToJoin);

    Map<Integer, Objects> directIndex = Base.toMap(listOfObjects);
    Function<Objects, Integer> reverseIndex = Functions.forMap(Base.inverse(directIndex));
    Function<Objects, Injection<Object, Pair<Integer, Object>>> objectToInjection = reverseIndex.then(Functions.forList(BasePair.Pair(du.unionSet(), du.injections()).y()));
    final Sets.FactorSet factorset = new Sets.FactorSet(BasePair.Pair(du.unionSet(), du.injections()).x());
    Map<Objects, TypelessSetMorphism> canonicalTSMPerObject = new HashMap<Objects, TypelessSetMorphism>();
    // have to factor the union by the equivalence relationship caused
    // by two morphisms mapping the same element to two possibly different.
    for (Objects o : domain().objects()) {
      Set from = nodesMorphism.apply(o);
      TypelessSetMorphism f = null;
      
      for (Arrows a : bundles.get(o)) {
        TypelessSetMorphism aAsMorphism = arrowsMorphism.apply(a);
        TypelessSetMorphism toUnion = TypelessSetMorphism.forFunction(aAsMorphism.codomain(), du.unionSet(), objectToInjection.apply(domain().d1(a)));  
        TypelessSetMorphism g = aAsMorphism.then(toUnion);
        canonicalTSMPerObject.put(o, g);
        if (f != null) {
          for (Object x : from) {
            factorset.merge(f.apply(x), g.apply(x));
          }
        }
        f = g;
      }
    }
    final TypelessSetMorphism factorMorphism = TypelessSetMorphism.forFactorset(factorset);
    Map<Objects, TypelessSetMorphism> coconeMap = factorMorphism.after().map(canonicalTSMPerObject);
    return new Cocone(factorset.factorset(), coconeMap);
  }
}
