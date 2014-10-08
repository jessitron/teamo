package teamo

import scala.concurrent.duration.FiniteDuration

//this difficulty probably becomes something more complicated later
//as different techs provide different difficulty challenges
class Project(val size: Double = 40.0, val difficulty:Double = 1.0)

//The codebase has certain implemented functionality, and a level of tech debt associated with it
case class Code(functionality:Double, debt:Double){
  def addWork(coder:Coder, culture:Culture, time:FiniteDuration):Code = this
}

class Task

class Work(val coder:Coder,val culture:Culture,val time:FiniteDuration)