package testing

import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAll

abstract class TestBase extends Specification with BeforeAll:
  args.execute(threadsNb = 4)

  def beforeAll(): Unit =
    TestWatch.start()
    val className = getClass.getSimpleName
    println(s"$className: ${TestWatch.timePassed}")
  
