package math.cat

import scalakittens.Result
import scalakittens.Result.OKif

/**
  * The data used in building an instance of Category
  */
private[cat] abstract class CategoryData(
  val graph: Graph) extends Graph {

  type Obj = Node
  type Objects = Set[Obj]
  override val name: String = "a category"

  private[cat] def asObj(x: Any): Obj = x.asInstanceOf[Obj]
  private[cat] def asArrow(a: Any): Arrow = a.asInstanceOf[Arrow]

  def obj(x: Any): Obj =
    Result.forValue(asObj(x)) filter (objects contains) getOrElse {
      throw new IllegalArgumentException(s"$x is not an object in $name")
    }

  def id(o: Obj): Arrow

  def m(f: Arrow, g: Arrow): Option[Arrow]

  override def validate: Result[CategoryData] = {
    val objectsHaveIds = OKif(!finiteNodes) orElse {
      Result.traverse(objects map {
        x ⇒
          val ux = id(x)
          OKif(d0(ux) == x, s"Domain of id $ux should be $x in $name") andAlso
            OKif(d1(ux) == x, s"Codomain of id $ux should be $x in $name")
      })
    }

    val idsAreNeutral = OKif(!finiteArrows) orElse {
      Result.traverse(arrows map { f ⇒
        val u_f = m(id(d0(f)), f)
        val f_u = m(f, id(d1(f)))
        OKif(u_f contains f, s"Left unit law broken for ${id(d0(f))} and $f: got $u_f in $name") andAlso
          OKif(f_u contains f, s"Right unit law broken for ${id(d1(f))} and $f: got $f_u in $name")
      })
    }

    val compositionsAreDefined = idsAreNeutral andThen OKif(!finiteArrows) orElse {
      Result.traverse {
        for {
          f ← arrows
          g ← arrows
          h = m(f, g)
        } yield {
          if (follows(g, f)) {
            Result(h) orCommentTheError s"composition must be defined for $f and $g in $name" flatMap { gf ⇒
              OKif(sameDomain(gf, f),
                s"Wrong composition $gf of $f and $g : its d0 is ${d0(gf)}, must be ${d0(f)} in $name") andAlso
                OKif(sameCodomain(gf, g),
                  s"Wrong composition $gf of $f and $g: its d1 is ${d1(gf)}, must be ${d1(g)} in $name")
            } returning()
          }
          else {
            OKif(h.isEmpty, s"Wrongly defined composition of $f and $g in $name")
          }
        }
      }
    }

    val compositionIsAssociative = compositionsAreDefined andThen (OKif(!finiteArrows) orElse {
      Result.traverse {
        for {
          f ← arrows
          g ← arrows
          h ← arrows
          gf ← m(f, g)
          hg ← m(g, h)
        } yield {
          val h_gf = m(gf, h)
          val hg_f = m(f, hg)
          // the following is for debugging
          val f0 = "" + f + ""
          OKif(h_gf == hg_f, s"Associativity broken for $f, $g and $h, got $h_gf vs $hg_f in $name")
        }
      }
    })

    objectsHaveIds andAlso compositionIsAssociative returning this
  }

  def objects: Objects = nodes

  lazy val listOfObjects = if (isFinite) objects.toList.sortBy(_.toString) else throw new IllegalStateException("Cannot sort infinite set")

  def nodes: Objects = graph.nodes.asInstanceOf[Objects]

  def arrows: Arrows = graph.arrows.asInstanceOf[Arrows]

  def d0(a: Arrow): Obj = {
    val gd0 = graph.d0(graph.arrow(a))
    obj(gd0)
  }

  def d1(a: Arrow): Obj = obj(graph.d1(graph.arrow(a)))

}
