package it.unito.planningFC

import it.unito.planningFC.taxiscenario.{Simulator}
import it.unito.planningFC.utils.UtilsPlanning

object PlanningFcApp extends App {

  val pathPlan :String = System.getProperty("user.dir") + "\\src\\main\\resources"
  val namePlan :String = "outputPlan"

  //PLANNING: (OPTIONAL if you already have the files in resources, you can comment the line )
  //Launch planning from Scala code to Docker, using POPF2 Planner which is in the container Docker
  //Input: container id (use docker ps -a) , domainS.pddl , problemS.pddl  (the latter two are in the resources folder)
  //Output files: outputPlan (which will appear in the resources folder)
  val pathProblem: String = System.getProperty("user.dir") + "\\src\\main\\resources"
  val nameProblem: String = "problemS.pddl"
  val idContainer : String = "d33" // REPLACE with your container id (docker ps -a)
  //UtilsPlanning.startPlanningOnDocker(pathProblem, nameProblem, idContainer)

  //SIMULATION:
  Simulator.startSimulationScenarioTaxi(pathPlan, namePlan, pathProblem, nameProblem)









}