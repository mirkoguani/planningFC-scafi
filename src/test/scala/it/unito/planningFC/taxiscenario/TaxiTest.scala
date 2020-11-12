package it.unito.planningFC.taxiscenario

import akka.actor.{ActorRef, ActorSystem, Props}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.must.Matchers
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.{Await, Future}

class TaxiTest extends  AnyFlatSpecLike with BeforeAndAfterAll with Matchers{

  "GetLocationT" should "initially be void" in {
    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    implicit val timeout = Timeout(5 seconds)
    val future = taxi ? Message.GetLocationT()
    val result = Await.result(future, timeout.duration).asInstanceOf[Location]
    system.terminate()
    assert(result.name == "")
  }

  "SetLocationT" should "sets the taxi's location" in {
    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    implicit val timeout = Timeout(5 seconds)
    var location : Location = new Location
    location.name = "h1"
    val future = taxi ? Message.SetLocationT(location)
    val result = Await.result(future, timeout.duration).asInstanceOf[Location]
    system.terminate()
    assert(result.name == "h1")

  }

  "GetPassengerInT" should "initially be void" in {
    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    implicit val timeout = Timeout(5 seconds)
    val future = taxi ? Message.GetPassengerInT()
    val result = Await.result(future, timeout.duration).asInstanceOf[String]
    system.terminate()
    assert(result == "")
  }

  "SetPassengerInT" should "sets that in the taxi there is a passenger  (or even no passengers)" in {
    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    implicit val timeout = Timeout(5 seconds)
    val future = taxi ? Message.SetPassengerInT("p1")
    val result = Await.result(future, timeout.duration).asInstanceOf[String]
    system.terminate()
    assert(result == "p1")
  }

  "Test StartActionDriveT" should "return true if works correctly" in {
    val action:String = "At time 0.0   START: (drive t2 d g3)"

    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)
    val locationFrom : Location = new Location
    locationFrom.name = "d"
    val locationTo : Location = new Location
    locationTo.name = "g3"
    locationTo.taxiIn = ""
    val locations : List[Location] = List(locationFrom, locationTo)
    val future = taxi ? Message.SetLocationT(locationFrom)
    var response :Location = Await.result(future, timeout.duration).asInstanceOf[Location]
    val drive : Drive = new Drive(action,locations)
    val futureAct = taxi ? Message.StartActionDriveT(drive)
    val responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    if (responseAct.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    system.terminate()
    assert(resultTest)
  }

  "Test EndActionDriveT" should "return true if works correctly" in {
    val action:String = "At time 10.0   END: (drive t2 d g3)"

    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)
    val locationFrom : Location = new Location()
    locationFrom.name = "d"
    val locationTo : Location = new Location()
    locationTo.name = "g3"
    val locations : List[Location] = List(locationFrom, locationTo)
    var future = taxi ? Message.SetLocationT(locationFrom)
    var response :Location = Await.result(future, timeout.duration).asInstanceOf[Location]
    var drive : Drive = new Drive(action,locations)
    var futureAct = taxi ? Message.EndActionDriveT(drive)
    var responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    if (responseAct.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    future = taxi ? Message.GetLocationT()
    response = Await.result(future, timeout.duration).asInstanceOf[Location]

    system.terminate()
    assert(resultTest && response.name.compareTo("g3") == 0)
  }

  "Test StartActionEnterT" should "return true if works correctly" in {
    val action:String = "At time 0.0   START: (enter p2 t1 l)"

    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)
    val location : Location = new Location
    location.name = "l"
    val locations : List[Location] = List(location)
    val future = taxi ? Message.SetLocationT(location)
    val response :Location = Await.result(future, timeout.duration).asInstanceOf[Location]
    val enter : Enter = new Enter(action,locations)
    val futureAct = taxi ? Message.StartActionEnterT(enter)
    val responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    if (responseAct.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    system.terminate()
    assert(resultTest)
  }

  "Test EndActionEnterT" should "return true if works correctly" in {
    val action:String = "At end 1.0   END: (enter p2 t1 l)"

    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)

    val location : Location = new Location
    location.name = "l"
    val locations : List[Location] = List(location)
    val future = taxi ? Message.SetLocationT(location)
    val response :Location = Await.result(future, timeout.duration).asInstanceOf[Location]
    val enter : Enter = new Enter(action,locations)
    var futureAct = taxi ? Message.EndActionEnterT(enter)
    var responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    if (responseAct.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    futureAct = taxi ? Message.GetPassengerInT()
    responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    system.terminate()
    assert(resultTest && responseAct.compareTo("p2") == 0)
  }

  "Test StartActionExitT" should "return true if works correctly" in {
    val action:String = "At time 20.0   START: (exit p2 t1 l)"

    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)
    val location : Location = new Location
    location.name = "l"
    var futureAct = taxi ? Message.SetPassengerInT("p2")
    var responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    val locations : List[Location] = List(location)
    val future = taxi ? Message.SetLocationT(location)
    val response :Location = Await.result(future, timeout.duration).asInstanceOf[Location]
    val exit : Exit = new Exit(action,locations)
    futureAct = taxi ? Message.StartActionExitT(exit)
    responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    if (responseAct.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    system.terminate()
    assert(resultTest)
  }

  "Test EndActionExitT" should "return true if works correctly" in {
    val action:String = "At end 21.0   END: (exit p2 t1 l)"

    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)
    val location : Location = new Location
    location.name = "l"
    val locations : List[Location] = List(location)
    val future = taxi ? Message.SetLocationT(location)
    val response :Location = Await.result(future, timeout.duration).asInstanceOf[Location]
    var futureAct = taxi ? Message.SetPassengerInT("p2")
    var responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    val exit : Exit = new Exit(action,locations)
    futureAct = taxi ? Message.EndActionExitT(exit)
    responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]
    if (responseAct.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    futureAct = taxi ? Message.GetPassengerInT()
    responseAct = Await.result(futureAct, timeout.duration).asInstanceOf[String]

    system.terminate()
    assert(resultTest && responseAct.compareTo("") == 0)
  }


}
