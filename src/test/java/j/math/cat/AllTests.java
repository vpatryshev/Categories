package j.math.cat;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Wraps all tests to run (have to list manually).
 * 
 * @author Vlad Patryshev
 *  * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>

 * 
 */
public class AllTests {
  private static final String packageName = AllTests.class.getPackage().getName();

  /**
   * Builds a test suite.
   * 
   * @param suiteName name of test suite
   * @param testNames names of tests, comma-separated
   * @return a test suite with all the tests added
   * @throws ClassNotFoundException when the test is not found
   */
  public static TestSuite suite(String suiteName, String testNames) throws ClassNotFoundException {
    TestSuite suite = new TestSuite(suiteName);
    for (String testName : testNames.split(",\\s*")) {
      suite.addTestSuite((Class<TestCase>)Class.forName(packageName + "." + testName + "Test"));
    }

    return suite;
  }

  /**
   * @return a test suite to run
   */
  public static Test suite() {
    try {
      return suite("All Tests",
          "Base, BinaryRelationship, Categories, Category, Functions, Functor, Graph, Maybe, Pair, PoSet, Predicate, SetDiagram, SetMorphism, Sets");
    } catch (ClassNotFoundException e) {
      throw new RuntimeException(e);
    }
  }
}
