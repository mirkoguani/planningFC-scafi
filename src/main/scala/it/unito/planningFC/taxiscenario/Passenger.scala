package it.unito.planningFC.taxiscenario

import akka.actor.Actor

case class StartActionEnterP(actionEnter : Enter)
case class EndActionEnterP(actionEnter : Enter)
case class StartActionExitP(actionExit : Exit)
case class EndActionExitP(actionExit : Exit)
case class GetLocationP()
case class GetInTaxiP()
case class GetLocationGoalP()
case class SetLocationP(location:Location)
case class SetInTaxiP(taxi:String)
case class SetLocationGoalP(location: Location)

class Passenger extends Actor {

  private var _location:Location = new Location
  private var _inTaxi: String = ""
  private var _xfreex: Boolean = true //if the taxi is free to be used (true) or busy in an action (false)
  private var _locationGoal:Location = new Location

  def location:Location = _location
  def location_=(location: Location) : Unit = {
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

  def locationGoal:Location = _locationGoal
  def locationGoal_=(locationGoal: Location) : Unit = {
    _locationGoal = locationGoal
  }

  override def receive: Receive = {
    case StartActionEnterP(actionEnter : Enter) => {
      // ensuring that the taxi is empty and the passenger is in that location
      if (inTaxi.compareTo("") == 0 && xfreex && _location.name.compareTo(actionEnter.location.name)==0) {
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The passenger " + actionEnter.passenger + " is already in a taxi (taxi " + inTaxi +") or he isn't in that location"
      }
    }

    case EndActionEnterP(actionEnter : Enter) => {
      xfreex = true
      inTaxi = actionEnter.taxi
      sender() ! "OK"
    }

    case StartActionExitP(actionExit : Exit) => {
      if (inTaxi.compareTo(actionExit.taxi) == 0 && xfreex && locationGoal.name.compareTo(actionExit.location.name) ==0) {
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The passenger " + actionExit.passenger + " isn't in taxi " + inTaxi + " or the location to get out is not the location goal"
      }
    }

    case EndActionExitP(actionExit : Exit) => {
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
    case GetLocationGoalP () => {
      sender() ! locationGoal
    }

    case SetLocationP (location : Location) => {
      _location = location
      sender() ! location
    }

    case SetInTaxiP (taxi :String) => {
      _inTaxi = taxi
      sender() ! inTaxi
    }

    case SetLocationGoalP (locationGoal : Location) => {
      _locationGoal = locationGoal
      sender() ! locationGoal
    }

  }
}