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


object Simulator {
  var system: ActorSystem = _
  var taxis: List[String] = List.empty
  var passengers: List[String] = List.empty
  var locations: List[Location] = List.empty
  var actorsTaxi : Array[ActorRef] = _
  var actorsPassengers : Array[ActorRef]  = _
  var actionsFEL: FEL = _
  var planList:List[String] = List.empty
  var listFEL: List[String] = List.empty
  val log: Logger = Logger.getLogger(this.getClass.getName)
  var makespan :Double = 0.000
  var finalsLocation:List[String] = List.empty


  def startSimulationScenarioTaxi (pathPlan: String, namePlan:String, pathProblem: String, nameProblem:String) :Unit = {
    setupSimulation(pathPlan: String, namePlan:String, pathProblem: String, nameProblem:String)

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
    //    val actorSimulator = system.actorOf(Props[Simulator](),"Simulator")
    taxis = UtilsPlanning.taxiFromProblem(pathProblem, nameProblem)
    passengers = UtilsPlanning.passengersFromProblem(pathProblem, nameProblem)
    actorsTaxi = new Array[ActorRef](taxis.length)
    actorsPassengers = new Array[ActorRef](passengers.length)
    for(i <- taxis.indices){
      val actorTaxi : ActorRef = system.actorOf(Props[Taxi](), taxis(i))
      actorsTaxi(i) = actorTaxi
    }
    for(i <- passengers.indices){
      val actorPassenger : ActorRef = system.actorOf(Props[Passenger](), passengers(i))
      actorsPassengers(i) = actorPassenger
    }
    locations = UtilsPlanning.locationsFromProblem(pathProblem,nameProblem)

    fromInitToTaxiPassengers(pathProblem, nameProblem)

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
    if (!action.contains("END_SIMULATION")) {
      var actionParts: Array[String] = action.split(" ")
      makespan = actionParts(2).toDouble
      log.info(action)

      actionHandlerTaxis(action)
      actionHandlerPassengers(action)

    } //end if !END_SIMULATION

    if (action.contains("END_SIMULATION")) {
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
          //actorsTaxi(i) ! SendStartAction(action, actorSimulator)
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

}


