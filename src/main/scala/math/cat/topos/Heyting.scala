package math.cat.topos

class Heyting[Obj](val topos: Topos) {
  type GP = Obj => Obj
  lazy val False: Obj = ???
  lazy val True: Obj = ???
//  val ∧ : ((topos.Ω.type, topos.Ω.type) => topos.Ω.type) = ???
//  val v : (topos.Ω.type, topos.Ω.type) => topos.Ω.type = ???
//  val → : (topos.Ω.type, topos.Ω.type) => topos.Ω.type = ???
//  val ¬ : (topos.Ω.type => topos.Ω.type) = ???
}
