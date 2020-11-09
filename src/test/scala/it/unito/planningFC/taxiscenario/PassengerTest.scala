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

class PassengerTest  extends  AnyFlatSpecLike with BeforeAndAfterAll with Matchers{

  "GetLocationP" should "gets the passenger's location" in {
    val system = ActorSystem("SimulationActorSystem")
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    implicit val timeout = Timeout(5 seconds)
    val future = passenger ? GetLocationP()
    val result = Await.result(future, timeout.duration).asInstanceOf[String]
    system.terminate()
    assert(result == "")
  }

  "SetLocationP" should "sets the passenger's location" in {
    val system = ActorSystem("SimulationActorSystem")
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    implicit val timeout = Timeout(5 seconds)
    val future = passenger ? SetLocationP("l1")
    val result = Await.result(future, timeout.duration).asInstanceOf[String]
    system.terminate()
    assert(result == "l1")
  }

  "GetInTaxiP" should "gets which passenger is in the taxi" in {
    val system = ActorSystem("SimulationActorSystem")
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    implicit val timeout = Timeout(5 seconds)
    val future = passenger ? GetInTaxiP()
    val result = Await.result(future, timeout.duration).asInstanceOf[String]
    system.terminate()
    assert(result == "")
  }

  "SetInTaxiP" should "sets that a passenger is in a taxi (or even void)" in {
    val system = ActorSystem("SimulationActorSystem")
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    implicit val timeout = Timeout(5 seconds)
    val future = passenger ? SetInTaxiP("t1")
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
    var future = passenger ? SetLocationP("l")
    var response :String = Await.result(future, timeout.duration).asInstanceOf[String]
    future = passenger ? StartActionEnterP(action)
    response = Await.result(future, timeout.duration).asInstanceOf[String]
    if (response.compareTo("OK") != 0) {
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
    var future = passenger ? SetLocationP("l")
    var response :String = Await.result(future, timeout.duration).asInstanceOf[String]
    future = passenger ? EndActionEnterP(action)
    response = Await.result(future, timeout.duration).asInstanceOf[String]
    if (response.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    future = passenger ? GetInTaxiP()
    response = Await.result(future, timeout.duration).asInstanceOf[String]

    system.terminate()
    assert(resultTest && response.compareTo("t1") == 0)
  }

  "Test StartActionExitP" should "return true if works correctly" in {
    val action:String = "At time 20.0   START: (exit p2 t1 l)"

    val system = ActorSystem("SimulationActorSystem")
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)
    var future = passenger ? SetLocationP("l")
    var response :String = Await.result(future, timeout.duration).asInstanceOf[String]
    future = passenger ? SetInTaxiP("t1")
    response = Await.result(future, timeout.duration).asInstanceOf[String]
    future = passenger ? StartActionExitP(action)
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
    val passenger : ActorRef = system.actorOf(Props[Passenger](),"Passenger")
    var resultTest : Boolean = false
    implicit val timeout = Timeout(5 seconds)
    var future = passenger ? SetLocationP("l")
    var response :String = Await.result(future, timeout.duration).asInstanceOf[String]
    future = passenger ? EndActionExitP(action)
    response = Await.result(future, timeout.duration).asInstanceOf[String]
    if (response.compareTo("OK") != 0) {
      resultTest = false
    } else {
      resultTest = true
    }
    future = passenger ? GetInTaxiP()
    response = Await.result(future, timeout.duration).asInstanceOf[String]

    system.terminate()
    assert(resultTest && response.compareTo("") == 0)
  }

}
