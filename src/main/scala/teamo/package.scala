import scala.concurrent.duration._

package object teamo {

  type Slack = Double

   val timeRatio:Double = 1.second / 1.day

  // put in own file?
   object Timing {
      def scale(calendarTime: Duration): Duration = {
         calendarTime * timeRatio
      }
   }

}
