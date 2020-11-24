package it.unito.planningFC.taxiscenario

import akka.actor.Actor
import it.unito.planningFC.taxiscenario.MyIncarnationAP.{EXPORT, ID, NSNS}

class Passenger extends Actor {

  var location:Location = new Location
  var inTaxi: String = ""
  var xfreex: Boolean = true //if the taxi is free to be used (true) or busy in an action (false)
  var locationGoal:Location = new Location
  var selfID: Int = -1

  var neighborhood : Set[ID] = Set()
  var exports : Map[ID,EXPORT]= Map()
  var LSNS : Map[String,Any]= Map()
  var NSNS : Map[NSNS,Map[ID,Any]] = Map()


  override def receive: Receive = {
    case Message.StartActionEnterP(actionEnter : Enter) => {
      // ensuring that the taxi is empty and the passenger is in that location
      if (inTaxi.compareTo("") == 0 && xfreex && location.name.compareTo(actionEnter.location.name)==0) {
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The passenger " + actionEnter.passenger + " is already in a taxi (taxi " + inTaxi +") or he isn't in that location"
      }
    }

    case Message.EndActionEnterP(actionEnter : Enter) => {
      xfreex = true
      inTaxi = actionEnter.taxi
      sender() ! "OK"
    }

    case Message.StartActionExitP(actionExit : Exit) => {
      if (inTaxi.compareTo(actionExit.taxi) == 0 && xfreex && locationGoal.name.compareTo(actionExit.location.name) == 0) {
        xfreex = false
        sender() ! "OK"
      } else {
        sender() ! "ERROR! Something went wrong! The passenger " + actionExit.passenger + " isn't in taxi " + inTaxi + " or the location to get out is not the location goal"
      }
    }

    case Message.EndActionExitP(actionExit : Exit) => {
      xfreex = true
      inTaxi = ""
      location = actionExit.location
      sender() ! "OK"
    }

    case Message.GetLocationP () => {
      sender() ! location
    }

    case Message.GetInTaxiP () => {
      sender() ! inTaxi
    }
    case Message.GetLocationGoalP () => {
      sender() ! locationGoal
    }

    case Message.GetSelfIDP () => {
      sender() ! selfID
    }


    case Message.SetLocationP (loc : Location) => {
      location = loc
      sender() ! location
    }

    case Message.SetInTaxiP (taxi :String) => {
      inTaxi = taxi
      sender() ! inTaxi
    }

    case Message.SetLocationGoalP (locGoal : Location) => {
      locationGoal = locGoal
      sender() ! locationGoal
    }

    case Message.SetSelfIDP (id : Int) => {
      selfID = id
      sender() ! selfID
    }

    case Message.ReceiveNeighborhoodP(neighborhoodP : Set[ID]) => {
      neighborhood = neighborhoodP
      sender() ! "OK"
    }
    case Message.ReceiveExportsP(exportsP: Map[ID,EXPORT]) => {
      exports = exportsP
      sender() ! "OK"
    }

    case Message.ReceiveLSNSP(lsnsP : Map[String,Any]) => {
      LSNS = lsnsP
      sender() ! "OK"
    }

    case Message.ReceiveNSNSP(nsnsP: Map[NSNS,Map[ID,Any]]) => {
      NSNS = nsnsP
      sender() ! "OK"
    }

    case Message.RunAggregateProgramP() => {
      var aggregateProgram : GradientAggregateProgram = new GradientAggregateProgram()
      var export : EXPORT = aggregateProgram.roundAggregateProgram(selfID, exports, LSNS, NSNS)
      sender() ! export
    }

  }
}