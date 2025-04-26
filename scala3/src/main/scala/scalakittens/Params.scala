package scalakittens

object Params:
  def config(name: String, alt: String): String =
    val p1 = Option(System.getenv(name))
    val p2 = Option(System.getProperty(name))
    val p: String = p1 orElse p2 getOrElse alt
    p

  def flag(name: String, alt: Boolean = false): Boolean =
    config(name, alt.toString).toBooleanOption getOrElse alt

  lazy val fullCheck: Boolean = flag("FullCheck")
  lazy val verbose:   Boolean = flag("Verbose")
  lazy val debug:     Boolean = flag("Debug")
  lazy val profile:   Boolean = flag("Profile")
  lazy val trace:     Boolean = flag("Trace")

  def debug(s: String): Unit =
    if (debug) then
      println(s"DEBUG:$s")
  def verbose(s: String): Unit =
    if (verbose) then println(s"VERBOSE:$s")