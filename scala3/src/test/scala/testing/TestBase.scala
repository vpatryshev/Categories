package testing

import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAll

abstract class TestBase extends Specification with BeforeAll:
  def beforeAll(): Unit = {
    TestWatch.start()
    val className = getClass.getSimpleName
    println(s"$className: ${TestWatch.timePassed}")
  }
  
