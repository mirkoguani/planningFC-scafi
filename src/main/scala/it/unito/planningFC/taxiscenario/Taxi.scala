package it.unito.planningFC.taxiscenario

import akka.actor.Actor
import it.unito.planningFC.taxiscenario.Message



class Taxi extends Actor {

  var location:Location = new Location
  var passengerIn: String = ""
  var xfreex: Boolean = true //if the taxi is free to be used (true) or busy in an action (false)

  override def receive: Receive = {

    case Message.StartActionDriveT(actionDrive: Drive) => {

      //ensuring that the taxi location is the same in the action
      if(location.name.compareTo(actionDrive.locationFrom.name) == 0 && xfreex && actionDrive.locationTo.taxiIn.compareTo("") ==0) {
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The taxi location does not match the Drive location or taxi is busy in another action"
      }
    }

    case Message.EndActionDriveT(actionDrive : Drive) => {
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

    case Message.StartActionEnterT(actionEnter : Enter) => {
      // ensuring that the taxi is empty
      if (passengerIn.compareTo("") == 0) {
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The taxi " + actionEnter.taxi + " isn't empty, there is already the passenger " + passengerIn + " inside"
      }
    }

    case Message.EndActionEnterT(actionEnter : Enter) => {
      xfreex = true
      passengerIn = actionEnter.passenger
      sender() ! "OK"
    }

    case Message.StartActionExitT(actionExit : Exit) => {
      // ensuring that the passenger is in the taxi
      if (passengerIn.compareTo(actionExit.passenger) == 0) {
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The taxi " + actionExit.taxi + " doesn't contain that passenger but " + passengerIn
      }
    }

    case Message.EndActionExitT(actionExit : Exit) => {
      xfreex = true
      passengerIn = ""
      sender() ! "OK"
    }


    case Message.GetLocationT () => {
      sender() ! location
    }

    case Message.GetPassengerInT () => {
      sender() ! passengerIn
    }

    case Message.SetLocationT (loc : Location) => {
      location = loc
      sender() ! location
    }

    case Message.SetPassengerInT (passenger :String) => {
      passengerIn = passenger
      sender() ! passengerIn
    }

  }
}
