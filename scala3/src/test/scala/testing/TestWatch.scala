package testing


import java.time.{Duration, Instant}

object TestWatch:

  private var t0: Instant = Instant.now
  private var started: Boolean = false
  
  def start(): Unit = synchronized {
    if !started then restart()
  }
  
  def restart(): Unit = synchronized {
    t0 = Instant.now
    started = true
  }

  def dt: Duration = if started then Duration.between(t0, Instant.now) else Duration.ZERO
  
  def timePassedSec: String =
    val diff = dt
    String.format("%02d.%03d", diff.toSeconds, diff.toMillisPart)
  
  def timePassed: String =
    val diff = dt
    String.format("%d:%02d:%02d.%03d", diff.toHours, diff.toMinutesPart, diff.toSecondsPart, diff.toMillisPart)
