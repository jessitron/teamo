package teamo

import scala.concurrent.duration.FiniteDuration

//this difficulty probably becomes something more complicated later
//as different techs provide different difficulty challenges
class TeaMo(val size: Double = 40.0, val difficulty:Double = 1.0){
  def addWork(coder:Coder, culture:Culture, time:FiniteDuration):TeaMo = this

}

class Task

class Work(val coder:Coder,val culture:Culture,val time:FiniteDuration)