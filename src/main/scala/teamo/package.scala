import scala.concurrent.duration._

package object teamo {

  type Slack = Double

   val timeRatio = 1.second / 1.day

  // put in own file?
   object Timing {
      def scale(calendarTime: FiniteDuration): FiniteDuration = {
         calendarTime * timeRatio
      }
   }

}
