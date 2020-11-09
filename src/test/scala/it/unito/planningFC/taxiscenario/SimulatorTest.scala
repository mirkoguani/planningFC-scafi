package it.unito.planningFC.taxiscenario

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.must.Matchers
import scala.language.postfixOps

class SimulatorTest extends  AnyFlatSpecLike with BeforeAndAfterAll with Matchers{

  "Simulation" should "end in the goal state" in {
    val taxis: Array[String] = Array("t1", "t2", "t3");
    val passengers: Array[String] = Array("p1", "p2", "p3", "p4", "p5", "p6", "p7")
    val pathPlan: String = System.getProperty("user.dir") + "\\src\\main\\resources"
    val namePlan: String = "outp21";

    Simulator.startSimulationScenarioTaxi(pathPlan, namePlan, taxis, passengers)

    var target: List[String] = List("location t1 : e", "location t2 : d", "location t3 : c", "location p1 : h2", "location p2 : h3", "location p3 : c", "location p4 : c", "location p5 : d", "location p6 : h3", "location p7 : e")

    for (i <- target.indices) {
      assert(target(i).compareTo(Simulator.finalsLocation(i)) == 0)
    }
  }



}




