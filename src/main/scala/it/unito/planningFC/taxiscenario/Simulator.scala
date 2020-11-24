package it.unito.planningFC.taxiscenario

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import it.unito.planningFC.utils.UtilsPlanning
import org.apache.log4j.Logger
import it.unito.planningFC.utils.FEL
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import it.unito.planningFC.taxiscenario.Simulator.actionsFEL
import it.unito.planningFC.utils.UtilsPlanning.{initsFromProblem, locationsStrFromProblem}

import MyIncarnationAP._

object Simulator {
  var system: ActorSystem = _
  var taxis: List[String] = List.empty
  var passengers: List[String] = List.empty
  var locations: List[Location] = List.empty
  var actorsTaxi : Array[ActorRef] = _
  var actorsPassengers : Array[ActorRef]  = _
  var actorsLocations : List[ActorRef] = List.empty
  var idTaxiAggrActors : Array[Int] = _
  var idPassengerAggrActors : Array[Int] = _
  var idLocationsAggrActors : Array[Int] = _
  var actionsFEL: FEL = _
  var planList:List[String] = List.empty
  var listFEL: List[String] = List.empty
  val log: Logger = Logger.getLogger(this.getClass.getName)
  var makespan :Double = 0.000
  var finalsLocation:List[String] = List.empty

  //For Aggregate Programming:
  var neighborhoodByDevice : Map[ID, Set[ID]] = Map()
  var exportsByDevice : Map[ID,Map[ID,EXPORT]] = Map() //each device has the exports of neighboring devices
  var LSNSByDevice : Map[ID,Map[String,Any]] = Map()
  var NSNSByDevice : Map[ID,Map[NSNS,Map[ID,Any]]] = Map()
  var counterIdAggrActors : Int = 0
  var broadcastFrequency : Double = 1.00
  var newExportsByDevice : Map[ID,EXPORT]= Map()


  def startSimulationScenarioTaxi (pathPlan: String, namePlan:String, pathProblem: String, nameProblem:String) :Unit = {
    setupSimulation(pathPlan: String, namePlan:String, pathProblem: String, nameProblem:String)

    actionsFEL.addBroadcastEventsInFEL(broadcastFrequency)

    listFEL = actionsFEL.FELtoList()
    for (action <- listFEL){
      actionHandler(action)
    }
    val goalReached : Boolean = isReachedGoalState(pathProblem, nameProblem)
    if(goalReached){
      log.info("GOAL STATE OF THE SIMULATION : REACHED")
    } else {
      log.warn("GOAL STATE OF THE SIMULATION : NOT REACHED!!!")
    }
    system.terminate() //terminates ActorSystem
  }

  private def setupSimulation (pathPlan: String, namePlan:String, pathProblem: String, nameProblem:String): Unit ={
    system = ActorSystem("SimulationActorSystem")
    taxis = UtilsPlanning.taxiFromProblem(pathProblem, nameProblem)
    passengers = UtilsPlanning.passengersFromProblem(pathProblem, nameProblem)
    actorsTaxi = new Array[ActorRef](taxis.length)
    actorsPassengers = new Array[ActorRef](passengers.length)
    idTaxiAggrActors = new Array[Int](taxis.length)
    idPassengerAggrActors = new Array[Int](passengers.length)

    for(i <- taxis.indices){
      val actorTaxi : ActorRef = system.actorOf(Props[Taxi](), taxis(i))
      actorsTaxi(i) = actorTaxi
    }
    for(i <- passengers.indices){
      val actorPassenger : ActorRef = system.actorOf(Props[Passenger](), passengers(i))
      actorsPassengers(i) = actorPassenger
    }

    locationsActorsFromProblem(pathProblem, nameProblem) //get the locations
    idLocationsAggrActors = new Array[Int](locations.length)

    fromInitToTaxiPassengers(pathProblem, nameProblem)
    setupAggregateActors()

    actionsFEL= new FEL //Future Event List (FEL)

    log.info("PlanFromFile")
    planList = UtilsPlanning.planFromFile(pathPlan,namePlan)
    for (actionString <- planList){
      log.info(actionString)
    }

    log.info("printFEL")
    actionsFEL.FELSimultationFromPlan(planList)
    actionsFEL.printEventsFEL()
  }



  private def actionHandler(action: String): Unit = {
    if (!action.contains("END_SIMULATION") && !action.contains("BROADCAST") ) {
      var actionParts: Array[String] = action.split(" ")
      makespan = actionParts(2).toDouble
      log.info(action)

      actionHandlerTaxis(action)
      actionHandlerPassengers(action)

    } //end if !END_SIMULATION

    if (action.contains("BROADCAST")){
      actionHandlerBroadcast()
    }

    if (action.contains("END_SIMULATION")) {

      //Aggregate Results:
      log.info("FINAL AGGREGATE RESULTS SIMULATION")
      for(i <- taxis.indices){
        log.info("EXPORT device taxi " + taxis(i) + " : " + newExportsByDevice(idTaxiAggrActors(i)))
      }
      for(i <- passengers.indices){
        log.info("EXPORT device passenger " + passengers(i) + " : " + newExportsByDevice(idPassengerAggrActors(i)))
      }
      for(i <- locations.indices){
        log.info("EXPORT device location " + locations(i).name + " : " + newExportsByDevice(idLocationsAggrActors(i)))
      }

      //Ask all the actors for their location
      log.info("FINAL STATE SIMULATION")
      log.info("Time: " + makespan)
      implicit val timeout = Timeout(5 seconds)
      for (i <- taxis.indices) {
        val future = actorsTaxi(i) ? Message.GetLocationT()
        val response = Await.result(future, timeout.duration).asInstanceOf[Location]
        log.info("at " + taxis(i) + " " + response.name)
        locations = locations
        finalsLocation = finalsLocation ::: List("at " + taxis(i) + " " + response.name)
      }
      for (i <- passengers.indices) {
        val future = actorsPassengers(i) ? Message.GetLocationP()
        val response = Await.result(future, timeout.duration).asInstanceOf[Location]
        log.info("at " + passengers(i) + " " + response.name)
        finalsLocation = finalsLocation ::: List("at " + passengers(i) + " " + response.name)
      }

    }
  }

  private def actionHandlerTaxis (action: String): Unit ={
    for (i <- taxis.indices) { //cycle on all taxis to see which of them is involved
      if (action.contains(taxis(i))) {
        if (action.contains("START")) {
          implicit val timeout = Timeout(5 seconds)
          //taxi start drive
          if (action.contains("drive")) {
            val drive : Drive = new Drive(action, locations)
            val future = actorsTaxi(i) ? Message.StartActionDriveT(drive)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
            if (response.compareTo("OK") != 0) {
              log.error(response)
              throw new RuntimeException (response)
            }
          }
          //taxi start enter
          if (action.contains("enter")) {
            val enter : Enter = new Enter(action, locations)
            val future = actorsTaxi(i) ? Message.StartActionEnterT(enter)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
            if (response.compareTo("OK") != 0) {
              log.error(response)
              throw new RuntimeException (response)
            }
          }
          //taxi start exit
          if (action.contains("exit")) {
            val exit : Exit = new Exit(action, locations)
            val future = actorsTaxi(i) ? Message.StartActionExitT(exit)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
            if (response.compareTo("OK") != 0) {
              log.error(response) //prints the error
              throw new RuntimeException (response)
            }
          }
        }

        if (action.contains("END")) {
          implicit val timeout = Timeout(5 seconds)
          if (action.contains("drive")) {
            val drive : Drive = new Drive(action, locations)
            val future = actorsTaxi(i) ? Message.EndActionDriveT(drive)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
          }
          if (action.contains("enter")) {
            val enter : Enter = new Enter(action, locations)
            val future = actorsTaxi(i) ? Message.EndActionEnterT(enter)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
          }
          if (action.contains("exit")) {
            val exit : Exit = new Exit(action, locations)
            val future = actorsTaxi(i) ? Message.EndActionExitT(exit)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
          }
        }
      }
    }
  }

 private def actionHandlerPassengers (action:String): Unit ={
    for (i <- passengers.indices) { //cycle on all passengers to see which of them is involved
      if (action.contains(passengers(i))) {
        if(action.contains("START")) {
          implicit val timeout = Timeout(5 seconds)
          if(action.contains("enter")) {
            val enter : Enter = new Enter(action, locations)
            val future = actorsPassengers(i) ? Message.StartActionEnterP(enter)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
            if(response.compareTo("OK") != 0){
              log.error(response) //prints the error
              throw new RuntimeException (response)
            }
          }
          if(action.contains("exit")) {
            val exit : Exit = new Exit(action, locations)
            val future = actorsPassengers(i) ? Message.StartActionExitP(exit)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
            if(response.compareTo("OK") != 0){
              log.error(response) //prints the error
              throw new RuntimeException (response)
            }
          }
        }
        if(action.contains("END")) {
          implicit val timeout = Timeout(5 seconds)
          if(action.contains("enter")){
            val enter : Enter = new Enter(action, locations)
            val future = actorsPassengers(i) ? Message.EndActionEnterP(enter)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
          }
          if(action.contains("exit")){
            val exit : Exit = new Exit(action, locations)
            val future = actorsPassengers(i) ? Message.EndActionExitP(exit)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
          }

        }
      }
    }
  }

  private def actionHandlerBroadcast() : Unit = {
    log.debug("BROADCAST HANDLER : ")

    updateNeighbohroodDevices()

    implicit val timeout = Timeout(5 seconds)
    for (i<-taxis.indices){
      //neighborhood
      var future = actorsTaxi(i) ? Message.ReceiveNeighborhoodT(neighborhoodByDevice(idTaxiAggrActors(i)))
      var response = Await.result(future, timeout.duration).asInstanceOf[String]
      //exports
      future = actorsTaxi(i) ? Message.ReceiveExportsT(exportsByDevice(idTaxiAggrActors(i)))
      response = Await.result(future, timeout.duration).asInstanceOf[String]
      //LSNS
      future = actorsTaxi(i) ? Message.ReceiveLSNST(LSNSByDevice(idTaxiAggrActors(i)))
      response = Await.result(future, timeout.duration).asInstanceOf[String]
      //NSNS
      future = actorsTaxi(i) ? Message.ReceiveNSNST(NSNSByDevice(idTaxiAggrActors(i)))
      response = Await.result(future, timeout.duration).asInstanceOf[String]

      val futureAP = actorsTaxi(i) ? Message.RunAggregateProgramT()
      val responseAP = Await.result(futureAP, timeout.duration).asInstanceOf[EXPORT]
      log.debug(responseAP)
      newExportsByDevice += (idTaxiAggrActors(i) -> responseAP)
    }

    for (i<-passengers.indices){
      //neighborhood
      var future = actorsPassengers(i) ? Message.ReceiveNeighborhoodP(neighborhoodByDevice(idPassengerAggrActors(i)))
      var response = Await.result(future, timeout.duration).asInstanceOf[String]
      //exports
      future = actorsPassengers(i) ? Message.ReceiveExportsP(exportsByDevice(idPassengerAggrActors(i)))
      response = Await.result(future, timeout.duration).asInstanceOf[String]
      //LSNS
      future = actorsPassengers(i) ? Message.ReceiveLSNSP(LSNSByDevice(idPassengerAggrActors(i)))
      response = Await.result(future, timeout.duration).asInstanceOf[String]
      //NSNS
      future = actorsPassengers(i) ? Message.ReceiveNSNSP(NSNSByDevice(idPassengerAggrActors(i)))
      response = Await.result(future, timeout.duration).asInstanceOf[String]

      val futureAP = actorsPassengers(i) ? Message.RunAggregateProgramP()
      val responseAP = Await.result(futureAP, timeout.duration).asInstanceOf[EXPORT]
      log.debug(responseAP)
      newExportsByDevice += (idPassengerAggrActors(i) -> responseAP)
    }

    for (i<-locations.indices){
      //neighborhood
      var future = actorsLocations(i) ? Message.ReceiveNeighborhoodL(neighborhoodByDevice(idLocationsAggrActors(i)))
      var response = Await.result(future, timeout.duration).asInstanceOf[String]
      //exports
      future = actorsLocations(i) ? Message.ReceiveExportsL(exportsByDevice(idLocationsAggrActors(i)))
      response = Await.result(future, timeout.duration).asInstanceOf[String]
      //LSNS
      future = actorsLocations(i) ? Message.ReceiveLSNSL(LSNSByDevice(idLocationsAggrActors(i)))
      response = Await.result(future, timeout.duration).asInstanceOf[String]
      //NSNS
      future = actorsLocations(i) ? Message.ReceiveNSNSL(NSNSByDevice(idLocationsAggrActors(i)))
      response = Await.result(future, timeout.duration).asInstanceOf[String]

      val futureAP = actorsLocations(i) ? Message.RunAggregateProgramL()
      val responseAP = Await.result(futureAP, timeout.duration).asInstanceOf[EXPORT]
      log.debug(responseAP)
      newExportsByDevice += (idLocationsAggrActors(i) -> responseAP)
    }

  }

  private def fromInitToTaxiPassengers (pathProblem : String , nameProblem : String) : Unit = {
    var inits: List[String] = UtilsPlanning.initsFromProblem(pathProblem,nameProblem)
    implicit val timeout = Timeout(5 seconds)
    for (init <- inits){
      //preposition : at
      if(init.contains("at")){
        var parts : Array[String] = init.split(" ")
        // add taxi to the Location
        for (i <- taxis.indices){
          if (parts(1).compareTo(taxis(i)) ==0){
            for(location <- locations){
              if(parts(2).compareTo(location.name)==0){
                location.taxiIn = taxis(i)
                val future = actorsTaxi(i) ? Message.SetLocationT(location)
                val response = Await.result(future, timeout.duration).asInstanceOf[Location]
              }
            }
          }
        }
        for (i <- passengers.indices){
          if (parts(1).compareTo(passengers(i)) ==0){
            for(location <- locations){
              if(parts(2).compareTo(location.name)==0){
                location.addPassengerIn(passengers(i))
                val future = actorsPassengers(i) ? Message.SetLocationP(location)
                val response = Await.result(future, timeout.duration).asInstanceOf[Location]
              }
            }
          }
        }
      }

      //preposition: empty
      if(init.contains("empty")){
        var parts : Array[String] = init.split(" ")
        for (i <- taxis.indices){
          if (parts(1).compareTo(taxis(i)) ==0){
            val future = actorsTaxi(i) ? Message.SetPassengerInT("")
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
          }
        }
      }
      //preposition: goal-of
      if(init.contains("goal-of")){
        var parts : Array[String] = init.split(" ")
        for(i <- passengers.indices) {
          if (parts(1).compareTo(passengers(i)) == 0) {
            for (location <- locations) {
              if (parts(2).compareTo(location.name) == 0) {
                val future = actorsPassengers(i) ? Message.SetLocationGoalP(location)
                val response = Await.result(future, timeout.duration).asInstanceOf[Location]
              }
            }
          }
        }
      }
    }
  }

  def isReachedGoalState (pathProblem: String, nameProblem: String): Boolean ={
    var goalsProbl : List[String] = UtilsPlanning.goalsFromProblem(pathProblem, nameProblem)
    // at t l
    // at p l
    var goalReached : Boolean = false
    for(goalProbl <- goalsProbl){
      goalReached = false
      for (goalSimul <- Simulator.finalsLocation){
        if(goalProbl.compareTo(goalSimul) == 0){
          goalReached = true
        }
      }
      if(!goalReached){ //if even only one goal was not reached then false
        return false
      }
    }
    return goalReached
  }

  def setupAggregateActors (): Unit ={
    implicit val timeout = Timeout(5 seconds)

    for (i <- taxis.indices){
      counterIdAggrActors = counterIdAggrActors + 1
      var selfID = counterIdAggrActors
      val future = actorsTaxi(i) ? Message.SetSelfIDT(selfID)
      val response = Await.result(future, timeout.duration).asInstanceOf[ID]
      idTaxiAggrActors(i) = selfID
      if (response != selfID){
        log.error("Error caused by SetSelfId Taxi!")
        new RuntimeException("Error caused by SetSelfId Taxi!")
      }
    }

    for (i <- passengers.indices){
      counterIdAggrActors = counterIdAggrActors + 1
      var selfID = counterIdAggrActors
      val future = actorsPassengers(i) ? Message.SetSelfIDP(selfID)
      val response = Await.result(future, timeout.duration).asInstanceOf[ID]
      idPassengerAggrActors(i) = selfID
      if (response != selfID){
        log.error("Error caused by SetSelfId Passenger!")
        new RuntimeException("Error caused by SetSelfId Passenger!")
      }
    }

    for (i <- locations.indices){
      counterIdAggrActors = counterIdAggrActors + 1
      var selfID = counterIdAggrActors
      val future = actorsLocations(i) ? Message.SetSelfIDL(selfID)
      val response = Await.result(future, timeout.duration).asInstanceOf[ID]
      idLocationsAggrActors(i) = selfID
      if (response != selfID){
        log.error("Error caused by SetSelfId Location!")
        new RuntimeException("Error caused by SetSelfId Location!")
      }
    }

    //id devices ranges from 1 to taxis.length + passengers.length + locations.length
    for(i <- 1 to taxis.length + passengers.length + locations.length){
      exportsByDevice += (i -> Map())
    }

  }

  def updateNeighbohroodDevices():Unit = {
    for (i <- taxis.indices) {
      var selfID: ID = idTaxiAggrActors(i)
      var nbrRangeMap : Map[ID,Any] = Map(selfID -> 0.00)
      //prepares a taxi's neighborhood, which is given by the location where he is and
      //the passengers in that location
      var neighborhood: Set[ID] = Set(selfID)
      //taxi's location:
      for (j <- locations.indices) {
        if (locations(j).taxiIn.contains(taxis(i))) {
          //taxi is in this location
          neighborhood += idLocationsAggrActors(j)
          nbrRangeMap += (idLocationsAggrActors(j) -> 0.00)
          //does the location passengers?
          for (p <- locations(j).passengersIn) {
            //let's look for the id of that passenger
            for (k <- passengers.indices) {
              if (passengers(k).compareTo(p) == 0) {
                neighborhood += idPassengerAggrActors(k)
                nbrRangeMap += (idPassengerAggrActors(k) -> 0.00)
              }
            }
          }
        }
      }
      neighborhoodByDevice += (selfID -> neighborhood)
      LSNSByDevice += (selfID -> Map("typeDevice" -> "taxi" , "locationSource" -> false))
      NSNSByDevice += (selfID -> Map("nbrRange" -> nbrRangeMap))
    }

    for (i <- passengers.indices){
      var selfID : ID = idPassengerAggrActors(i)
      var nbrRangeMap : Map[ID,Any] = Map(selfID -> 0.00)
      //prepares a passenger's neighborhood, which is given by the location where he is,
      //the other passengers in that location and the taxis in that location
      var neighborhood : Set[ID] = Set(selfID)
      //passenger's location:
      for (j <- locations.indices){
        if(locations(j).passengersIn.contains(passengers(i))){
          //passenger is in this location
          neighborhood += idLocationsAggrActors(j)
          nbrRangeMap += (idLocationsAggrActors(j) -> 0.00)
          //does the location have other passengers?
          for (p <- locations(j).passengersIn){
            if(p.compareTo(passengers(i)) != 0){ //if it is another passenger
              //let's look for the id of that passenger
              for(k <- passengers.indices){
                if(passengers(k).compareTo(p) == 0){
                  neighborhood += idPassengerAggrActors(k)
                  nbrRangeMap += (idPassengerAggrActors(k) -> 0.00)
                }
              }
            }
          }
          //does the location have a taxi?
          if (locations(j).taxiIn.compareTo("")!= 0){
            //let's look for the id of that taxi
            for(k <- taxis.indices){
              if(taxis(k).compareTo(locations(j).taxiIn) == 0){
                neighborhood += idTaxiAggrActors(k)
                nbrRangeMap += (idTaxiAggrActors(k) -> 0.00)
              }
            }
          }

        }
      }
      neighborhoodByDevice += (selfID -> neighborhood)
      LSNSByDevice += (selfID -> Map("typeDevice" -> "passenger", "locationSource" -> false))
      NSNSByDevice += (selfID -> Map("nbrRange" -> nbrRangeMap))
    }

    for (i <- locations.indices){
      var selfID : ID = idLocationsAggrActors(i)
      var nbrRangeMap : Map[ID,Any] = Map(selfID -> 0.00)
      //prepare the neighborhood of a location, which is given by the IDs of the other nearby locations.
      var neighborhood : Set[Int] =  Set(selfID)
      for ( j <- locations.indices){
        if (i!= j){
          if(locations(i).isDirectlyConnected(locations(j))){
            neighborhood += idLocationsAggrActors(j)
            nbrRangeMap += (idLocationsAggrActors(j) -> 10.00)
          }
        }
      }
      neighborhoodByDevice += ( selfID -> neighborhood)
      if(locations(i).name.compareTo("d")==0){
        LSNSByDevice += (selfID -> Map("typeDevice" -> "location" , "locationSource" -> true))
      } else {
        LSNSByDevice += (selfID -> Map("typeDevice" -> "location", "locationSource" -> false))
      }
      NSNSByDevice += (selfID -> Map("nbrRange" -> nbrRangeMap))
    }

    //and now that the neighborhood of each device has been updated, update exportsByDevice
    for(i <- taxis.indices){
      //exportsByDevice += (idTaxiAggrActors(i) -> Map()) //remove the previous export
      var neighbors : Set[ID] = neighborhoodByDevice(idTaxiAggrActors(i))
      var exports : Map[ID, EXPORT] = Map()
      for (idNeighbor <- neighbors){
        if(newExportsByDevice.contains(idNeighbor)) {
          exports += (idNeighbor -> newExportsByDevice(idNeighbor))
        }
      }
      exportsByDevice += (idTaxiAggrActors(i) -> exports)
    }
    for(i <- passengers.indices){
      var neighbors : Set[ID] = neighborhoodByDevice(idPassengerAggrActors(i))
      var exports : Map[ID, EXPORT] = Map()
      for (idNeighbor <- neighbors){
        if(newExportsByDevice.contains(idNeighbor)) {
          exports += (idNeighbor -> newExportsByDevice(idNeighbor))
        }
      }
      exportsByDevice += (idPassengerAggrActors(i) -> exports)
    }

    for(i <- locations.indices){
      var neighbors : Set[ID] = neighborhoodByDevice(idLocationsAggrActors(i))
      var exports : Map[ID, EXPORT] = Map()
      for (idNeighbor <- neighbors){
        if(newExportsByDevice.contains(idNeighbor)) {
          exports += (idNeighbor -> newExportsByDevice(idNeighbor))
        }
      }
      exportsByDevice += (idLocationsAggrActors(i) -> exports)
    }


  }


  def locationsActorsFromProblem (pathProblem : String, nameProblem : String) : Unit = {
    var locationsStr : List[String] = locationsStrFromProblem(pathProblem, nameProblem)

    // From the names of the locations we create the Location instances and ActorLocation
    var countLocations : Int = 0
    for (locationName <- locationsStr) {
      var location: Location = new Location()
      location.name = locationName
      locations = locations ::: List(location)
      //create the actor Location
      var actorLocation : ActorRef = system.actorOf(Props[LocationActor](), locations(countLocations).name)
      actorsLocations = actorsLocations ::: List(actorLocation)
      //send message, to the actor, to save the name
      implicit val timeout = Timeout(5 seconds)
      val future = actorsLocations(countLocations) ? Message.SetNameL(locationName)
      val response = Await.result(future, timeout.duration).asInstanceOf[String]
      log.debug("actorsLocations ("+countLocations+") name = " + locationName)
      countLocations = countLocations + 1
    }
    //Using the problemS.pddl we fill in the other fields of the Location instances
    var inits:List[String] = initsFromProblem(pathProblem,nameProblem)
    //we are interested in prepositions: at , free, directly-connected
    for (init <- inits){
      //preposition : at
      if(init.contains("at")){
        var parts : Array[String] = init.split(" ")
        // add taxi to the Location
        if(parts(1).startsWith("t")){
          for (i <- locations.indices){
            if(parts(2).compareTo(locations(i).name) == 0){
              locations(i).taxiIn = parts(1)
              implicit val timeout = Timeout(5 seconds)
              val future = actorsLocations(i) ? Message.SetTaxiInL(parts(1))
              val response = Await.result(future, timeout.duration).asInstanceOf[String]
            }
          }
        }
        if(parts(1).startsWith("p")){
          //for (location <- locations){
          for (i <- locations.indices){
            if(parts(2).compareTo(locations(i).name) == 0){
              locations(i).addPassengerIn(parts(1))
              implicit val timeout = Timeout(5 seconds)
              val future = actorsLocations(i) ? Message.AddPassengerInL(parts(1))
              val response = Await.result(future, timeout.duration).asInstanceOf[List[String]]

            }
          }
        }
      }
      //preposition : free
      if(init.contains("free")){
        var parts : Array[String] = init.split(" ")
        for (i <- locations.indices){
          if(parts(1).compareTo(locations(i).name) == 0){
            locations(i).free = true
            implicit val timeout = Timeout(5 seconds)
            val future = actorsLocations(i) ? Message.SetFreeL(true)
            val response = Await.result(future, timeout.duration).asInstanceOf[Boolean]
            if (response != true){
              throw new RuntimeException("ERROR! Error setting free location")
            }
          }
        }
      }
      //preposition : directly-connected
      if(init.contains("directly-connected")){
        var parts : Array[String] = init.split(" ")
        //for (locationFrom <- locations){
        for (i <- locations.indices){ //locationFrom
          if(parts(1).compareTo(locations(i).name) == 0){
            for (locationTo <- locations){
              if(parts(2).compareTo(locationTo.name) == 0){
                locations(i).addConnectedLocation(locationTo)
                implicit val timeout = Timeout(5 seconds)
                val future = actorsLocations(i) ? Message.AddConnectedLocationL(locationTo)
                val response = Await.result(future, timeout.duration).asInstanceOf[List[Location]]
              }
            }
          }
        }
      }
    }

  }




}


