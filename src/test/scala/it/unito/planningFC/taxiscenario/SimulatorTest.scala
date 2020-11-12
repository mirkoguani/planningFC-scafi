package it.unito.planningFC.taxiscenario

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.must.Matchers
import scala.language.postfixOps

class SimulatorTest extends  AnyFlatSpecLike with BeforeAndAfterAll with Matchers{

  "Simulation" should "ends in the goal state" in {
    val pathPlan: String = System.getProperty("user.dir") + "\\src\\main\\resources"
    val namePlan: String = "outputPlan";
    val pathProblem: String = System.getProperty("user.dir") + "\\src\\main\\resources"
    val nameProblem: String = "problemS.pddl";

    Simulator.startSimulationScenarioTaxi(pathPlan, namePlan, pathProblem, nameProblem)

    assert(Simulator.isReachedGoalState(pathProblem,nameProblem))
  }

}




