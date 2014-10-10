package teamo

import akka.actor.Actor

import scala.concurrent.duration.FiniteDuration

//this difficulty probably becomes something more complicated later
//as different techs provide different difficulty challenges

case class Work(coder:Coder,culture:Culture,time:FiniteDuration)

class TeaMo extends Actor{
  
  var size = 0.0
  var debt = 0.0
  
  def receive:Receive= {
    case w:Work => addWork(w)
  }
  
  def addWork(w:Work) ={}

}

class Task
