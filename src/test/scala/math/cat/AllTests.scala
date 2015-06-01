package math.cat


import org.scalatest.SuperSuite

/**
 * All tests.
 */
class AllTests extends SuperSuite(
    List(
      new CategorySuite,
      new FunctionsSuite,
      new GraphMorphismSuite,
      new GraphSuite,
      new PoSetMorphismSuite,
      new PoSetSuite,
      new SetMorphismSuite,
      new SetsSuite,
      new TypelessSetMorphismSuite
    )
  )