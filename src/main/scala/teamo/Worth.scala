package teamo

case class Worth(implementedFeatures: Value, workingFeatures: Value = 0, outagePercent: Double = 0) {
  assert(implementedFeatures >= 0)
  assert(workingFeatures >= 0)
  assert(workingFeatures <= implementedFeatures)
  assert(outagePercent <= 1)
  assert(outagePercent >= 0)

  def currentValue = workingFeatures * (1 - outagePercent)

  def break(thisMuch: Value) = copy(workingFeatures = Math.min(0, workingFeatures - thisMuch))

  def outage(additionalPercentOff: Double) = {
    val wasWorking = 1 - outagePercent
    copy(outagePercent = outagePercent + wasWorking * additionalPercentOff)
  }
}
