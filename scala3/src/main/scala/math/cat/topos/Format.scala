package math.cat.topos

object Format:

  def shortTitle(s: String): String =
    s.replaceAll(s"\\s*Diagram\\w*\\[([^\\]])*]", "").replace("Set()", "{}")
