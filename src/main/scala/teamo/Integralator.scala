package teamo

import scala.io.Source


object Integralator {

  def main(args: Array[String]) {
    args.foreach { fn =>
      val lines = Source.fromFile(fn).getLines().drop(1)map(_.split(','))
      val value = lines.foldLeft((0,0d,0d)){ case ((time,lastVal,total),line) =>
        val newTime = line(0).toInt
        val newVal = line(5).toDouble
        (newTime, newVal,total + lastVal * (newTime-time))
      }._3
      val bdvalue = "%.2f" format value
      println(s"$fn - $bdvalue")
    }
  }
}
