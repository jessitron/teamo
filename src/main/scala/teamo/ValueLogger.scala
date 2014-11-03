package teamo;

import java.util.Date
import java.io.FileWriter

trait ValueLogger {
  def loginate(featureCount: Int, featuresValue: Value,
               problemCount: Int, problemsValue: Value,
               overallValue: Value,codebaseSize:Double): Unit
}

case class PirateLogger() extends ValueLogger {
  def loginate(featureCount: Int, featuresValue: Value,
               problemCount: Int, problemsValue: Value,
               overallValue: Value,codebaseSize:Double) {}
}

class FileLogger(filename: String) extends ValueLogger {

  val writer = new FileWriter(filename)
  writer.write("millis, featureCount, featuresVal, problemCount, problemsVal, totalVal, codeSize\n")
  val start: Long = new Date().getTime

  def loginate(featureCount: Int, featuresValue: Value,
               problemCount: Int, problemsValue: Value,
               overallValue: Value,codebaseSize:Double) {

      writer.write("%d,%d,%f,%d,%f,%f,%f\n".format(timeSinceStart,featureCount,featuresValue,
        problemCount, overallValue-featuresValue, overallValue,codebaseSize))
  }

  private def timeSinceStart:Long = new Date().getTime - start

  def close() = writer.close()

}
