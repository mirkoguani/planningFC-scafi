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
  var actorsTaxi : Array[ActorRef] = _
  var actorsPassengers : Array[ActorRef]  = _
  var actionsFEL: FEL = _
  var planList:List[String] = List.empty
  var listFEL: List[String] = List.empty
  val log = Logger.getLogger(this.getClass.getName)
  var makespan :Double = 0.000
  var finalsLocation:List[String] = List.empty


  def startSimulationScenarioTaxi (pathPlan: String, namePlan:String, taxis : Array[String], passengers: Array[String]) :Unit = {
    setupSimulation(pathPlan: String, namePlan:String, taxis : Array[String], passengers: Array[String])

    listFEL = actionsFEL.FELtoList()
    for (action <- listFEL){
      actionHandler(action,taxis,passengers)
    }
    system.terminate() //terminates ActorSystem
  }

  private def setupSimulation (pathPlan: String, namePlan:String, taxis : Array[String], passengers: Array[String]): Unit ={
    system = ActorSystem("SimulationActorSystem")
    //    val actorSimulator = system.actorOf(Props[Simulator](),"Simulator")
    actorsTaxi = new Array[ActorRef](taxis.length)
    actorsPassengers = new Array[ActorRef](passengers.length)
    for(i <- taxis.indices){
      val actorTaxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi"+taxis(i))
      actorsTaxi(i) = actorTaxi
    }
    for(i <- passengers.indices){
      val actorPassenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger" + passengers(i))
      actorsPassengers(i) = actorPassenger
    }

    //FEL
    actionsFEL= new FEL

    log.info("PlanFromFile")
    planList = UtilsPlanning.planFromFile(pathPlan,namePlan)
    for (actionString <- planList){
      log.info(actionString)
    }

    log.info("printFEL")
    actionsFEL.FELSimultationFromPlan(planList)
    actionsFEL.printEventsFEL()
  }

  private def actionHandler(action: String, taxis: Array[String], passengers: Array[String]): Unit = {
    if (!action.contains("END_SIMULATION")) {
      var actionParts: Array[String] = action.split(" ")
      makespan = actionParts(2).toDouble
      log.info(action)

      actionHandlerTaxis(action, taxis)
      actionHandlerPassengers(action, passengers)

    } //end if !END_SIMULATION

    if (action.contains("END_SIMULATION")) {
      //Ask all the actors for their location
      log.info("FINAL STATE SIMULATION")
      log.info("Time: " + makespan)
      implicit val timeout = Timeout(5 seconds)
      for (i <- taxis.indices) {
        val future = actorsTaxi(i) ? GetLocationT()
        val response = Await.result(future, timeout.duration).asInstanceOf[String]
        log.info("location " + taxis(i) + " : " + response)
        finalsLocation = finalsLocation ::: List("location " + taxis(i) + " : " + response)
      }
      for (i <- passengers.indices) {
        val future = actorsPassengers(i) ? GetLocationP()
        val response = Await.result(future, timeout.duration).asInstanceOf[String]
        log.info("location " + passengers(i) + " : " + response)
        finalsLocation = finalsLocation ::: List("location " + passengers(i) + " : " + response)
      }
    }
  }

  private def actionHandlerTaxis (action: String, taxis : Array[String]): Unit ={
    for (i <- taxis.indices) { //cycle on all taxis to see which of them is involved
      if (action.contains(taxis(i))) {
        if (action.contains("START")) {
          //actorsTaxi(i) ! SendStartAction(action, actorSimulator)
          implicit val timeout = Timeout(5 seconds)
          //taxi start drive
          if (action.contains("drive")) {
            val future = actorsTaxi(i) ? StartActionDriveT(action)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
            if (response.compareTo("OK") != 0) {
              log.warn(response)
              throw new RuntimeException (response)
            }
          }
          //taxi start enter
          if (action.contains("enter")) {
            val future = actorsTaxi(i) ? StartActionEnterT(action)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
            if (response.compareTo("OK") != 0) {
              log.warn(response)
              throw new RuntimeException (response)
            }
          }
          //taxi start exit
          if (action.contains("exit")) {
            val future = actorsTaxi(i) ? StartActionExitT(action)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
            if (response.compareTo("OK") != 0) {
              log.warn(response) //prints the error
              throw new RuntimeException (response)
            }
          }
        }

        if (action.contains("END")) {
          implicit val timeout = Timeout(5 seconds)
          if (action.contains("drive")) {
            val future = actorsTaxi(i) ? EndActionDriveT(action)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
          }
          if (action.contains("enter")) {
            val future = actorsTaxi(i) ? EndActionEnterT(action)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
          }
          if (action.contains("exit")) {
            val future = actorsTaxi(i) ? EndActionExitT(action)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
          }
        }
      }
    }
  }

 private def actionHandlerPassengers (action:String, passengers: Array[String]): Unit ={
    for (i <- passengers.indices) { //cycle on all passengers to see which of them is involved
      if (action.contains(passengers(i))) {
        if(action.contains("START")) {
          implicit val timeout = Timeout(5 seconds)
          if(action.contains("enter")) {
            val future = actorsPassengers(i) ? StartActionEnterP(action)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
            if(response.compareTo("OK") != 0){
              log.warn(response) //prints the error
              throw new RuntimeException (response)
            }
          }
          if(action.contains("exit")) {
            val future = actorsPassengers(i) ? StartActionExitP(action)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
            if(response.compareTo("OK") != 0){
              log.warn(response) //prints the error
              throw new RuntimeException (response)
            }
          }
        }
        if(action.contains("END")) {
          implicit val timeout = Timeout(5 seconds)
          if(action.contains("enter")){
            val future = actorsPassengers(i) ? EndActionEnterP(action)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
          }
          if(action.contains("exit")){
            val future = actorsPassengers(i) ? EndActionExitP(action)
            val response = Await.result(future, timeout.duration).asInstanceOf[String]
          }

        }
      }
    }
  }

}


