package it.unito.planningFC.taxiscenario

import akka.actor.{Actor}

case class StartActionDriveT(actionDrive : Drive)
case class EndActionDriveT(actionDrive : Drive)
case class StartActionEnterT(actionEnter : Enter)
case class EndActionEnterT(actionEnter : Enter)
case class StartActionExitT(actionExit : Exit)
case class EndActionExitT(actionExit : Exit)
case class GetLocationT ()
case class GetPassengerInT()
case class SetLocationT(location:Location)
case class SetPassengerInT(passenger:String)

class Taxi extends Actor {

  private var _location:Location = new Location
  private var _passengerIn: String = ""
  private var _xfreex: Boolean = true //if the taxi is free to be used (true) or busy in an action (false)

  def location:Location = _location
  def location_=(location: Location) :Unit = {
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

    case StartActionDriveT(actionDrive: Drive) => {
      //val actionDrive: Drive = new Drive(action)
      /*if (location.name.compareTo("") == 0) {
        //initialize first location (these rows will be deleted when init is completed)
        location.name = actionDrive.locationFrom
      } */
      //ensuring that the taxi location is the same in the action

      if(location.name.compareTo(actionDrive.locationFrom.name) == 0 && xfreex && actionDrive.locationTo.taxiIn.compareTo("") ==0) {
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The taxi location does not match the Drive location or taxi is busy in another action"
      }
    }

    case EndActionDriveT(actionDrive : Drive) => {
      xfreex = true
      location.taxiIn = "" //locationFrom
      location.free = true
      if(passengerIn.compareTo("") != 0){
        location.removePassengerIn(passengerIn)
      }
      location = actionDrive.locationTo //locationTo
      location.taxiIn = actionDrive.taxi
      if(passengerIn.compareTo("") != 0){
        location.addPassengerIn(passengerIn)
      }
      location.free = false
      sender() ! "OK"
    }

    case StartActionEnterT(actionEnter : Enter) => {
      // ensuring that the taxi is empty
      if (passengerIn.compareTo("") == 0) {
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The taxi " + actionEnter.taxi + " isn't empty, there is already the passenger " + passengerIn + " inside"
      }
    }

    case EndActionEnterT(actionEnter : Enter) => {
      xfreex = true
      passengerIn = actionEnter.passenger
      sender() ! "OK"
    }

    case StartActionExitT(actionExit : Exit) => {
      // ensuring that the passenger is in the taxi
      if (passengerIn.compareTo(actionExit.passenger) == 0) {
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The taxi " + actionExit.taxi + " doesn't contain that passenger but " + passengerIn
      }
    }

    case EndActionExitT(actionExit : Exit) => {
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

    case SetLocationT (location : Location) => {
      _location = location
      sender() ! location
    }

    case SetPassengerInT (passenger :String) => {
      _passengerIn = passenger
      sender() ! passenger
    }

  }
}
