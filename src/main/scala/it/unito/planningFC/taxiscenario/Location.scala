package it.unito.planningFC.taxiscenario

import akka.actor.Actor
import it.unito.planningFC.taxiscenario.MyIncarnationAP.{EXPORT, ID, NSNS}

class LocationActor extends Actor{
  var name:String = ""
  var taxiIn: String = ""
  var passengersIn: List[String] = List.empty //in a Location there can be more passengers but only one taxi
  var free: Boolean = false
  var connectedLocations : List[Location] = List.empty
  var selfID: Int = -1

  var neighborhood : Set[ID] = Set()
  var exports : Map[ID,EXPORT]= Map()
  var LSNS : Map[String,Any]= Map()
  var NSNS : Map[NSNS,Map[ID,Any]] = Map()

  override def receive: Receive = {

      case Message.GetNameL() => {
        sender() ! name
      }
      case Message.GetTaxiInL() => {
        sender() ! taxiIn
      }

      case Message.GetFreeL() => {
        sender() ! free
      }

      case Message.GetPassengersInL() => {
        sender() ! passengersIn
      }

      case Message.GetSelfIDL() => {
        sender() ! selfID
      }

      case Message.SetNameL(nameLoc: String) => {
        name = nameLoc
        sender() ! name
      }

      case Message.SetTaxiInL(taxi: String) => {
        taxiIn = taxi
        sender() ! taxiIn
      }

      case Message.SetFreeL(isFree: Boolean) => {
        free = isFree
        sender() ! free
      }

      case Message.SetSelfIDL (id : Int) => {
        selfID = id
        sender() ! selfID
      }

      case Message.SetPassengersInL(passengers: List[String]) => {
        passengersIn = passengers
        sender() ! passengersIn
      }

      case Message.AddPassengerInL(passenger: String) => {
        addPassengerIn(passenger)
        sender() ! passengersIn
      }

      case Message.AddConnectedLocationL(location: Location) => {
        addConnectedLocation(location)
        sender() ! connectedLocations
      }

      case Message.RemovePassengerInL(namePassenger: String) => {
        removePassengerIn(namePassenger)
        sender() ! passengersIn
      }

      case Message.IsDirectlyConnectedByNameL (nameLocation: String) => {
        sender() ! isDirectlyConnected(nameLocation)
      }

      case Message.IsDirectlyConnectedByLocL(location: Location) => {
        sender() ! isDirectlyConnected(location)
      }

      case Message.ReceiveNeighborhoodL(neighborhoodL : Set[ID]) => {
        neighborhood = neighborhoodL
        sender() ! "OK"
      }
      case Message.ReceiveExportsL(exportsL: Map[ID,EXPORT]) => {
        exports = exportsL
        sender() ! "OK"
      }

      case Message.ReceiveLSNSL(lsnsL : Map[String,Any]) => {
        LSNS = lsnsL
        sender() ! "OK"
      }

      case Message.ReceiveNSNSL(nsnsL: Map[NSNS,Map[ID,Any]]) => {
        NSNS = nsnsL
        sender() ! "OK"
      }

      case Message.RunAggregateProgramL() => {
        var aggregateProgram : GradientAggregateProgram = new GradientAggregateProgram()
        var export : EXPORT = aggregateProgram.roundAggregateProgram(selfID, exports, LSNS, NSNS)
        sender() ! export
      }


  }

  def addPassengerIn (passenger: String): Unit = {
    if (!passengersIn.contains(passenger)) {
      passengersIn = passengersIn ::: List(passenger)
    }
  }

  def removePassengerIn (namePassenger: String) : Unit = {
    var passengers : List[String] = List.empty
    for (passenger <- passengersIn){
      if (passenger.compareTo(namePassenger) != 0){
        passengers = passengers ::: List(passenger)
      }
    }
    passengersIn = passengers
  }

  def addConnectedLocation (location: Location) : Unit = {
    var found : Boolean = false
    for (loc <- connectedLocations){
      if (loc.name.compareTo(location.name) == 0){
        found = true
      }
    }
    if(!found) {
      connectedLocations = connectedLocations ::: List(location)
    }
  }

  def isDirectlyConnected(nameLocation : String): Boolean ={
    for (location <- connectedLocations){
      if(location.name.compareTo(nameLocation) == 0){
        return true
      }
    }
    return false
  }

  def isDirectlyConnected(location : Location): Boolean ={
    for (loc <- connectedLocations){
      if(loc.name.compareTo(location.name) == 0){
        return true
      }
    }
    return false
  }

  def getLocationFromActorLocation (): Location ={
    var location : Location = new Location()
    location.name = name
    location.connectedLocations = connectedLocations
    location.passengersIn = passengersIn
    location.taxiIn = taxiIn
    location.free = free
    return location
  }


}

class Location {
  var name:String = ""
  var taxiIn: String = ""
  var passengersIn: List[String] = List.empty //in a Location there can be more passengers but only one taxi
  var free: Boolean = false
  var connectedLocations : List[Location] = List.empty

  def addPassengerIn (passenger: String): Unit = {
    if (!passengersIn.contains(passenger)) {
      passengersIn = passengersIn ::: List(passenger)
    }
  }

  def removePassengerIn (namePassenger: String) : Unit = {
    var passengers : List[String] = List.empty
    for (passenger <- passengersIn){
      if (passenger.compareTo(namePassenger) != 0){
        passengers = passengers ::: List(passenger)
      }
    }
    passengersIn = passengers
  }

  def addConnectedLocation (location: Location) : Unit = {
    var found : Boolean = false
    for (loc <- connectedLocations){
      if (loc.name.compareTo(location.name) == 0){
        found = true
      }
    }
    if(!found) {
      connectedLocations = connectedLocations ::: List(location)
    }
  }

  def isDirectlyConnected(nameLocation : String): Boolean ={
    for (location <- connectedLocations){
      if(location.name.compareTo(nameLocation) == 0){
        return true
      }
    }
    return false
  }

  def isDirectlyConnected(location : Location): Boolean ={
    for (loc <- connectedLocations){
      if(loc.name.compareTo(location.name) == 0){
        return true
      }
    }
    return false
  }

}
