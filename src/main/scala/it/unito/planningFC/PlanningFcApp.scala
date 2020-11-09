package it.unito.planningFC

import it.unito.planningFC.taxiscenario.Simulator

object PlanningFcApp extends App {

  val taxis : Array[String] = Array("t1","t2","t3");
  val passengers : Array[String] = Array("p1","p2","p3","p4","p5","p6","p7")
  val pathPlan :String = System.getProperty("user.dir") + "\\src\\main\\resources"
  val namePlan :String = "outp21";

  Simulator.startSimulationScenarioTaxi(pathPlan, namePlan, taxis, passengers)

}