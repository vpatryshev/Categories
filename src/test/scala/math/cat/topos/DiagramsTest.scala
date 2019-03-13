package math.cat.topos

import math.cat.{Category, TestDiagrams}
import org.specs2.mutable.Specification
import Category._
import math.Test

class DiagramsTest extends Test with TestDiagrams {

  type SUT = SmallDiagram
  
  "DiagramsTest" should {
    "id" in {
      val topos = new Diagrams(W)
      expect { d =>
        val idtrans = topos.id(d)
        idtrans.d0 === d
        idtrans.d1 === d
        // the following case fails miserably; investigate
        // java.lang.AbstractMethodError: Fatal execution error, caused by math.cat.NaturalTransformation
        // .transformPerObject(Ljava/lang/Object;)Ljava/lang/Object;
        //java.lang.AbstractMethodError: math.cat.NaturalTransformation.transformPerObject(Ljava/lang/Object;)
        // Ljava/lang/Object;
        //	at math.cat.NaturalTransformation.math$cat$NaturalTransformation$$comp$1(NaturalTransformation.scala:48)
        
//        idtrans.compose(idtrans) === idtrans
      } (SampleWDiagram.asDiagram)
    }

    "m" in {
      // todo: test composition
      val topos = new Diagrams(M)
      expect { d =>
        val idtrans = topos.id(d)
        idtrans.d0 === d
        idtrans.d1 === d
      } (SampleMDiagram.asDiagram)
    }

    "graphOfDiagrams" in {
      ok
    }

  }
}
