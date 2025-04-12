package scalakittens

object Params:
  def config(name: String, alt: String): String =
    val p1 = Option(System.getenv(name))
    val p2 = Option(System.getProperty(name))
    val p: String = p1 orElse p2 getOrElse alt
    p

  def flag(name: String, alt: Boolean = false): Boolean =
    config(name, alt.toString).toBooleanOption getOrElse alt

  def fullCheck: Boolean = flag("FullCheck", false)
  def verbose: Boolean = flag("Verbose", false)
  def debug: Boolean = flag("Debug", false)
  def profile: Boolean = flag("Profile", false)
  def trace: Boolean = flag("Trace", false)