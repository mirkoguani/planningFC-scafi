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
    val future = taxi ? GetLocationT()
    val result = Await.result(future, timeout.duration).asInstanceOf[String]
    system.terminate()
    assert(result == "")
  }

  "SetLocationT" should "sets the taxi's location" in {
    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    implicit val timeout = Timeout(5 seconds)
    val future = taxi ? SetLocationT("h1")
    val result = Await.result(future, timeout.duration).asInstanceOf[String]
    system.terminate()
    assert(result == "h1")
  }

  "GetPassengerInT" should "initially be void" in {
    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    implicit val timeout = Timeout(5 seconds)
    val future = taxi ? GetPassengerInT()
    val result = Await.result(future, timeout.duration).asInstanceOf[String]
    system.terminate()
    assert(result == "")
  }

  "SetPassengerInT" should "sets that in the taxi there is a passenger  (or even no passengers)" in {
    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    implicit val timeout = Timeout(5 seconds)
    val future = taxi ? SetPassengerInT("p1")
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
    val future = taxi ? StartActionDriveT(action)
    val response = Await.result(future, timeout.duration).asInstanceOf[String]
    if (response.compareTo("OK") != 0) {
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
    var future = taxi ? EndActionDriveT(action)
    var response = Await.result(future, timeout.duration).asInstanceOf[String]
    if (response.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    future = taxi ? GetLocationT()
    response = Await.result(future, timeout.duration).asInstanceOf[String]

    system.terminate()
    assert(resultTest && response.compareTo("g3") == 0)
  }

  "Test StartActionEnterT" should "return true if works correctly" in {
    val action:String = "At time 0.0   START: (enter p2 t1 l)"

    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)
    var future = taxi ? SetLocationT("l")
    var response :String = Await.result(future, timeout.duration).asInstanceOf[String]
    future = taxi ? StartActionEnterT(action)
    response = Await.result(future, timeout.duration).asInstanceOf[String]
    if (response.compareTo("OK") != 0) {
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
    var future = taxi ? SetLocationT("l")
    var response :String = Await.result(future, timeout.duration).asInstanceOf[String]
    future = taxi ? EndActionEnterT(action)
    response = Await.result(future, timeout.duration).asInstanceOf[String]
    if (response.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    future = taxi ? GetPassengerInT()
    response = Await.result(future, timeout.duration).asInstanceOf[String]

    system.terminate()
    assert(resultTest && response.compareTo("p2") == 0)
  }

  "Test StartActionExitT" should "return true if works correctly" in {
    val action:String = "At time 20.0   START: (exit p2 t1 l)"

    val system = ActorSystem("SimulationActorSystem")
    val taxi : ActorRef = system.actorOf(Props[Taxi](),"Taxi")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)
    var future = taxi ? SetLocationT("l")
    var response :String = Await.result(future, timeout.duration).asInstanceOf[String]
    future = taxi ? SetPassengerInT("p2")
    response = Await.result(future, timeout.duration).asInstanceOf[String]
    future = taxi ? StartActionExitT(action)
    response = Await.result(future, timeout.duration).asInstanceOf[String]
    if (response.compareTo("OK") != 0) {
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
    var future = taxi ? SetLocationT("l")
    var response :String = Await.result(future, timeout.duration).asInstanceOf[String]
    future = taxi ? EndActionExitT(action)
    response = Await.result(future, timeout.duration).asInstanceOf[String]
    if (response.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    future = taxi ? GetPassengerInT()
    response = Await.result(future, timeout.duration).asInstanceOf[String]

    system.terminate()
    assert(resultTest && response.compareTo("") == 0)
  }


}
