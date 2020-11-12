package it.unito.planningFC.taxiscenario

class Location {
  private var _name:String = ""
  private var _taxiIn: String = ""
  private var _passengersIn: List[String] = List.empty //in a Location there can be more passengers but only one taxi
  private var _free: Boolean = false
  private var _connectedLocations : List[Location] = List.empty

  def name:String = _name
  def name_=(name: String) : Unit = {
    _name = name
  }

  def taxiIn:String = _taxiIn
  def taxiIn_=(taxiIn: String) : Unit = {
    _taxiIn = taxiIn
  }

  def passengersIn:List[String] = _passengersIn
  def passengersIn_=(passengersIn: List[String]) : Unit = {
    _passengersIn = passengersIn
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
    _passengersIn = passengers
  }

  def free:Boolean = _free
  def free_=(free: Boolean) : Unit = {
    _free = free
  }

  def connectedLocations:List[Location] = _connectedLocations
  def connectedLocations_=(connectedLocations: List[Location]) : Unit = {
    _connectedLocations = connectedLocations
  }

  def addConnectedLocation (location: Location) : Unit = {
    var found : Boolean = false
    for (loc <- connectedLocations){
      if (loc.name.compareTo(location.name) == 0){
        found = true
      }
    }
    if(!found) {
      _connectedLocations = _connectedLocations ::: List(location)
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
