package it.unito.planningFC.taxiscenario

import akka.actor.{ActorRef, ActorSystem, Props}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.must.Matchers
import akka.pattern.ask
import akka.util.Timeout
import it.unito.planningFC.taxiscenario
import org.apache.log4j.Logger

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.{Await, Future}

class PassengerTest  extends  AnyFlatSpecLike with BeforeAndAfterAll with Matchers{
  val log: Logger = Logger.getLogger(this.getClass.getName)
  "GetLocationP" should "gets the passenger's location" in {
    val system = ActorSystem("SimulationActorSystem")
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    implicit val timeout = Timeout(5 seconds)
    val future = passenger ? Message.GetLocationP()
    val result = Await.result(future, timeout.duration).asInstanceOf[Location]
    system.terminate()
    assert(result.name == "")
  }

  "SetLocationP" should "sets the passenger's location" in {
    val system = ActorSystem("SimulationActorSystem")
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    implicit val timeout = Timeout(5 seconds)
    var location : Location = new Location
    location.name = "l1"
    val future = passenger ? Message.SetLocationP(location)
    val result = Await.result(future, timeout.duration).asInstanceOf[Location]
    system.terminate()
    assert(result.name == "l1")
  }

  "GetInTaxiP" should "gets which passenger is in the taxi" in {
    val system = ActorSystem("SimulationActorSystem")
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    implicit val timeout = Timeout(5 seconds)
    val future = passenger ? Message.GetInTaxiP()
    val result = Await.result(future, timeout.duration).asInstanceOf[String]
    system.terminate()
    assert(result == "")
  }

  "SetInTaxiP" should "sets that a passenger is in a taxi (or even void)" in {
    val system = ActorSystem("SimulationActorSystem")
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    implicit val timeout = Timeout(5 seconds)
    val future = passenger ? Message.SetInTaxiP("t1")
    val result = Await.result(future, timeout.duration).asInstanceOf[String]
    system.terminate()
    assert(result == "t1")
  }

  "Test StartActionEnterP" should "return true if works correctly" in {
    val action:String = "At time 0.0   START: (enter p2 t1 l)"

    val system = ActorSystem("SimulationActorSystem")
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)
    val location : Location = new Location()
    location.name = "l"
    val locations : List[Location] = List(location)
    val future = passenger ? Message.SetLocationP(location)
    var response :Location = Await.result(future, timeout.duration).asInstanceOf[Location]
    val enter : Enter = new Enter(action,locations)
    val futureAct = passenger ? Message.StartActionEnterP(enter)
    val responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    if (responseAct.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    system.terminate()
    assert(resultTest)
  }

  "Test EndActionEnterP" should "return true if works correctly" in {

    val action:String = "At end 1.0   END: (enter p2 t1 l)"

    val system = ActorSystem("SimulationActorSystem")
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)
    val location : Location = new Location()
    location.name = "l"
    val locations:List[Location] = List(location)
    var future = passenger ? Message.SetLocationP(location)
    var response :Location = Await.result(future, timeout.duration).asInstanceOf[Location]
    var futureAct = passenger ? Message.SetInTaxiP("t1")
    var responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    val enter : Enter = new Enter(action,locations)
    futureAct = passenger ?Message. EndActionEnterP(enter)
    responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    if (responseAct.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    futureAct = passenger ? Message.GetInTaxiP()
    responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    log.info(responseAct)
    system.terminate()
    assert(resultTest && responseAct.compareTo("t1") == 0)
  }

  "Test StartActionExitP" should "return true if works correctly" in {
    val action:String = "At time 20.0   START: (exit p2 t1 l)"

    val system = ActorSystem("SimulationActorSystem")
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)
    val location : Location = new Location()
    location.name = "l"
    val locations:List[Location] = List(location)
    var future = passenger ? Message.SetLocationP(location)
    var response :Location = Await.result(future, timeout.duration).asInstanceOf[Location]
    future = passenger ? Message.SetLocationGoalP(location)
    response = Await.result(future, timeout.duration).asInstanceOf[Location]
    var futureAct = passenger ? Message.SetInTaxiP("t1")
    var responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    var exit : Exit = new Exit(action, locations)
    futureAct = passenger ? Message.StartActionExitP(exit)
    responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    if (responseAct.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    system.terminate()
    assert(resultTest)
  }

  "Test EndActionExitP" should "return true if works correctly" in {
    val action:String = "At end 21.0   END: (exit p2 t1 l)"

    val system = ActorSystem("SimulationActorSystem")
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)
    val location : Location = new Location()
    location.name = "l"
    val locations:List[Location] = List(location)
    val future = passenger ? Message.SetLocationP(location)
    val response :Location = Await.result(future, timeout.duration).asInstanceOf[Location]
    var exit : Exit = new Exit(action, locations)
    var futureAct = passenger ? Message.EndActionExitP(exit)
    var responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    if (responseAct.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    futureAct = passenger ? Message.GetInTaxiP()
    responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]

    system.terminate()
    assert(resultTest && responseAct.compareTo("") == 0)
  }

}
