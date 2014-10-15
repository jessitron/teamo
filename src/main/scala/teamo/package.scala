import scala.concurrent.duration._

package object teamo {

  type Slack = Double /* between 0 and 1 */
  type CodeQuality = Double /* between 0 and 1 */
  type Value = Double /* not limited */

   val timeRatio:Double = 1.second / 1.day

  // put in own file?
   object Timing {
      def scale(calendarTime: Duration): Duration = {
         calendarTime * timeRatio
      }
      def wait(calendarTime: Duration) {
        scala.concurrent.blocking {
          Thread.sleep(scale(calendarTime).toMillis)
        }
      }
   }

}
