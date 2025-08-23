package math.cat.topos

/**
 * Using it for formatting strings
 */
object Format:

  def shortTitle(s: String): String =
    s.replaceAll(s"\\s*Diagram\\w*\\[([^\\]])*]", "").replace("Set()", "{}")
