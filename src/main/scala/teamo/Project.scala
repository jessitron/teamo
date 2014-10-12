package teamo

import akka.actor.Actor
import teamo.TeaMo.{TeaMoValue, GetValue}

import scala.concurrent.duration._
//this difficulty probably becomes something more complicated later
//as different techs provide different difficulty challenges

case class Work(startTime: FiniteDuration = 0.millis,
                coder:Coder,
                culture:Culture,
                time:FiniteDuration)

class TeaMo extends Actor{
  
  var size = 0.0
  var debt = 0.0
  
  def receive:Receive= {
    case w:Work => addWork(w)
    case GetValue => sender ! calculateValue
  }
  
  def calculateValue = {
    TeaMoValue(size-debt)
  }
  //this is horrific, but without a model we need something
  def addWork(w:Work) ={
     val workSize = w.time.toMinutes
     size += workSize
     debt += workSize/(1+w.culture.slack)
  }

}

object TeaMo{
  case object GetValue
  case class TeaMoValue(value:Double)
}

class Task
