package it.unito.planningFC.taxiscenario

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
        true
      }
    }
    false
  }

  def isDirectlyConnected(location : Location): Boolean ={
    for (loc <- connectedLocations){
      if(loc.name.compareTo(location.name) == 0){
        true
      }
    }
    false
  }

}
