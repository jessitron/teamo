package teamo

import TeamNature.ButtFactory
case class TeamNature(culture: Culture, membersCount: Int, featureSupplierFactory: ButtFactory) {

  def +(moreSlack: Slack) = copy(culture = Culture(Slack(culture.slack.value)))
}
object TeamNature {
  type ButtFactory = ()=>() => Feature
}

  /**
   * Slack is a multiple of development time,
   * which represents how much space people have to
   * improve the code and develop their skills while
   * working.
   *
   * It can't be lower than 0
   * It's a multiple of the task duration, so a reasonable
   * upper bound would be 3 or 5, around there.
   */
case class Slack(value: Double) {
  def >(other: Slack) = value > other.value // argh. feels like java
}

case class Culture(slack:Slack) {
}
