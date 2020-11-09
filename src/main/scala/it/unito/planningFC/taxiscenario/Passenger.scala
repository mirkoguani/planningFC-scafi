package it.unito.planningFC.taxiscenario

import akka.actor.{Actor}

case class StartActionEnterP(action: String)
case class EndActionEnterP(action: String)
case class StartActionExitP(action: String)
case class EndActionExitP(action: String)
case class GetLocationP()
case class GetInTaxiP()
case class SetLocationP(location:String)
case class SetInTaxiP(taxi:String)

class Passenger extends Actor {
  //val log = Logging(context.system, this)
  //log.debug("prova debug attore")

  private var _location:String = ""
  private var _inTaxi: String = ""
  private var _xfreex: Boolean = true //if the taxi is free to be used (true) or busy in an action (false)
  private var _locationGoal:String =""

  def location:String = _location
  def location_=(location: String) : Unit = {
    _location = location
  }

  def inTaxi:String = _inTaxi
  def inTaxi_=(inTaxi: String) :Unit = {
    _inTaxi = inTaxi
  }

  def xfreex:Boolean = _xfreex
  def xfreex_=(xfreex: Boolean) :Unit = {
    _xfreex = xfreex
  }

  def locationGoal:String = _locationGoal
  def locationGoal_=(locationGoal: String) : Unit = {
    _locationGoal = locationGoal
  }

  override def receive: Receive = {
    case StartActionEnterP(action) => {
      val actionEnter: Enter = new Enter(action)

      // ensuring that the taxi is empty
      if (inTaxi.compareTo("") == 0 && xfreex) {
        xfreex = false
        location = actionEnter.location
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The passenger " + actionEnter.passenger + " is already in a taxi, taxi " + inTaxi
      }
    }

    case EndActionEnterP(action) => {
      val actionEnter : Enter = new Enter(action)
      xfreex = true
      inTaxi = actionEnter.taxi
      sender() ! "OK"
    }

    case StartActionExitP(action) => {
      val actionExit: Exit = new Exit(action)
      if (inTaxi.compareTo(actionExit.taxi) == 0 && xfreex) {  // && locationGoal.compareTo(actionExit.location) ==0
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The passenger " + actionExit.passenger + " isn't in taxi " + inTaxi + " or the location to ge out is not the location goal"
      }
    }

    case EndActionExitP(action) => {
      val actionExit : Exit = new Exit(action)
      xfreex = true
      inTaxi = ""
      location = actionExit.location
      sender() ! "OK"
    }

    case GetLocationP () => {
      sender() ! location
    }

    case GetInTaxiP () => {
      sender() ! inTaxi
    }

    case SetLocationP (location : String) => {
      _location = location
      sender() ! location
    }

    case SetInTaxiP (taxi :String) => {
      _inTaxi = taxi
      sender() ! inTaxi
    }

  }
}