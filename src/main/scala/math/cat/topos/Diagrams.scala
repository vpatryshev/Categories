package math.cat.topos

import math.cat.{Category, Graph, SetDiagram}
import Diagrams._

abstract class Diagrams[C <: Category[_, _]]
  extends Category[Diagram[C], DiagramArrow[C]](graphOfDiagrams[C]) {
  type C0 = C#Objects
  type C1 = C#Arrow
  type Diagram = SetDiagram[C0, C1]
}

class DiagramArrow[C <: Category[_, _]]

object Diagrams {
  type Diagram[C <: Category[_, _]] = SetDiagram[C#Objects, C#Arrow]
//  type DiagramArrow[C <: Category[_, _]]
  def graphOfDiagrams[C <: Category[_, _]]: Graph[Diagram[C], DiagramArrow[C]] = {
    ???
  }
}
