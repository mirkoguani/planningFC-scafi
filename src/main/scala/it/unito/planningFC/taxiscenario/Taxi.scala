package it.unito.planningFC.taxiscenario

import akka.actor.{Actor, ActorRef}

case class StartActionDriveT(action: String)
case class EndActionDriveT(action: String)
case class StartActionEnterT(action: String)
case class EndActionEnterT(action: String)
case class StartActionExitT(action: String)
case class EndActionExitT(action: String)
case class GetLocationT ()
case class GetPassengerInT ()
case class SetLocationT(location:String)
case class SetPassengerInT(passenger:String)

class Taxi extends Actor {

  //val log = Logging(context.system, this)
  //log.debug("prova debug attore")
  private var _location:String = ""
  private var _passengerIn: String = ""
  private var _xfreex: Boolean = true //if the taxi is free to be used (true) or busy in an action (false)

  def location:String = _location
  def location_=(location: String) :Unit = {
    _location = location
  }

  def passengerIn:String = _passengerIn
  def passengerIn_=(passengerIn: String) :Unit = {
    _passengerIn = passengerIn
  }

  def xfreex:Boolean = _xfreex
  def xfreex_=(xfreex: Boolean) :Unit = {
    _xfreex = xfreex
  }

  override def receive: Receive = {

    case StartActionDriveT(action) => {
      val actionDrive: Drive = new Drive(action)
      if (location.compareTo("") == 0) {
        //initialize first location (these rows will be deleted when init is completed)
        location = actionDrive.locationFrom
      }
      //ensuring that the taxi location is the same in the action
      if(location.compareTo(actionDrive.locationFrom) == 0 && xfreex) {
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The taxi location does not match the Drive location or taxi is busy in another action"
      }
    }

    case EndActionDriveT(action) => {
      val actionDrive : Drive = new Drive(action)
      xfreex = true
      location = actionDrive.locationTo
      sender() ! "OK"
    }

    case StartActionEnterT(action) => {
      val actionEnter: Enter = new Enter(action)
      // ensuring that the taxi is empty
      if (passengerIn.compareTo("") == 0) {
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The taxi " + actionEnter.taxi + " isn't empty, there is already the passenger " + passengerIn + " inside"
      }
    }

    case EndActionEnterT(action) => {
      val actionEnter : Enter = new Enter(action)
      xfreex = true
      passengerIn = actionEnter.passenger
      sender() ! "OK"
    }

    case StartActionExitT(action) => {
      val actionExit: Exit = new Exit(action)
      // ensuring that the passenger is in the taxi
      if (passengerIn.compareTo(actionExit.passenger) == 0) {
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The taxi " + actionExit.taxi + " doesn't contain that passenger but " + passengerIn
      }
    }

    case EndActionExitT(action) => {
      val actionExit : Exit = new Exit(action)
      xfreex = true
      passengerIn = ""
      sender() ! "OK"
    }


    case GetLocationT () => {
      sender() ! location
    }

    case GetPassengerInT () => {
      sender() ! passengerIn
    }

    case SetLocationT (location : String) => {
      _location = location
      sender() ! location
    }

    case SetPassengerInT (passenger :String) => {
      _passengerIn = passenger
      sender() ! passenger
    }

  }
}
