package it.unito.planningFC.utils

import java.io.{File, FilenameFilter}
import java.util.Scanner

import it.unito.planningFC.taxiscenario.Location
import org.apache.log4j.Logger
import sys.process._
import scala.sys.process.ProcessLogger

object UtilsPlanning {

  val log: Logger = Logger.getLogger(this.getClass.getName)

  def planFromFile (pathPlan: String, namePlan: String) : List[String] = {
    var planList : List[String] = List.empty
    val f: File = new File(pathPlan)

    val filter: FilenameFilter = new FilenameFilter() {
      override def accept(dir: File, name: String): Boolean = {
        name.startsWith(namePlan)
      }
    }

    val pathnames: Array[String] = f.list(filter)
    var maxLastPlan: Int = 0
    var nameFilePlan: String = namePlan //default

    for (i <- pathnames.indices) {
      val pathname: String = pathnames(i)
      if (pathname.contains("\\.")) {
        val parts: Array[String] = pathname.split("\\.")
        val numPlan: Int = Integer.parseInt(parts(1))
        if (numPlan > maxLastPlan) {
          nameFilePlan = parts(0)
          maxLastPlan = numPlan
        }
      }
    }
    var myObj: File = null
    if (maxLastPlan > 0) {
      myObj = new File(pathPlan + "\\" + nameFilePlan + "." + maxLastPlan)
      log.debug("Plan: " +pathPlan + "\\"+nameFilePlan +"."+ maxLastPlan)
    }
    if(maxLastPlan == 0){
      myObj = new File(pathPlan + "\\" + nameFilePlan)
      log.debug("Plan: " +pathPlan + "\\"+nameFilePlan)
    }


    val myReader :Scanner = new Scanner(myObj)
    while (myReader.hasNextLine) {
      val data :String = myReader.nextLine()
      if(!data.startsWith(";")){ //ignores some comments row at the beginning of the Plan file
        planList = planList ::: List(data)
      }
    }
    myReader.close()

    return planList
  }

  def initsFromProblem(pathProblem : String, nameProblem : String): List[String] ={
    var nameFileProblem : String = nameProblem
    if(!nameProblem.endsWith(".pddl")){
      nameFileProblem = nameProblem + ".pddl"
    }
    var initList : List[String] = List.empty
    val myObj :File = new File(pathProblem + "\\" + nameFileProblem)
    //log.debug("Problem: " + pathProblem + "\\" + nameFileProblem)

    val myReader :Scanner = new Scanner(myObj)
    var initStarted : Boolean = false
    var initFinished : Boolean = false
    while (myReader.hasNextLine && !initFinished) {
      var data :String = myReader.nextLine()
      data = data.trim
      if(data.contains(":init") && !initStarted){
        initStarted = true
      }

      if(initStarted && !initFinished){
        if(data.startsWith(")")){
          initFinished = true
          initStarted = false
        }
      }

      if(initStarted){
        if(!data.contains(":init")) {
          data = data.replace("(", "")
          data = data.replace(")", "")
          initList = initList ::: List(data)
        }
      }
    }
    myReader.close()

    return initList
  }

  def goalsFromProblem(pathProblem : String, nameProblem : String): List[String] ={
    var nameFileProblem : String = nameProblem
    if(!nameProblem.endsWith(".pddl")){
      nameFileProblem = nameProblem + ".pddl"
    }
    var goalList : List[String] = List.empty
    val myObj :File = new File(pathProblem + "\\" + nameFileProblem)
    log.debug("Problem: " + pathProblem + "\\" + nameFileProblem)

    val myReader :Scanner = new Scanner(myObj)
    var goalStarted : Boolean = false
    var goalFinished : Boolean = false
    while (myReader.hasNextLine && !goalFinished) {
      var data :String = myReader.nextLine()
      data = data.trim
      if(data.contains(":goal") && !goalStarted){
        goalStarted = true
      }

      if(goalStarted && !goalFinished){
        if(data.startsWith(")")){
          goalFinished = true
          goalStarted = false
        }
      }

      if(goalStarted){
        if(!data.contains(":goal") && !data.contains("and")) {
          data = data.replace("(", "")
          data = data.replace(")", "")
          goalList = goalList ::: List(data)
        }
      }
    }
    myReader.close()

    return goalList
  }

  def objectsFromProblem(pathProblem : String, nameProblem : String): List[String] ={
    var nameFileProblem : String = nameProblem
    if(!nameProblem.endsWith(".pddl")){
      nameFileProblem = nameProblem + ".pddl"
    }
    var objectList : List[String] = List.empty
    val myObj :File = new File(pathProblem + "\\" + nameFileProblem)
    //log.debug("Problem: " + pathProblem + "\\" + nameFileProblem)

    val myReader :Scanner = new Scanner(myObj)
    var objectStarted : Boolean = false
    var objectFinished : Boolean = false
    while (myReader.hasNextLine && !objectFinished) {
      var data :String = myReader.nextLine()
      data = data.trim
      if(data.contains(":objects") && !objectStarted){
        objectStarted = true
      }

      if(objectStarted && !objectFinished){
        if(data.startsWith(")")){
          objectFinished = true
          objectStarted = false
        }
      }

      if(objectStarted){
        if(!data.contains(":objects")) {
          data = data.replace("(", "")
          data = data.replace(")", "")
          objectList = objectList ::: List(data)
        }
      }
    }
    myReader.close()

    return objectList
  }

  def taxiFromProblem (pathProblem : String, nameProblem : String):List[String]  = {
    var objects : List[String] = objectsFromProblem(pathProblem,nameProblem)
    var taxi : List[String] = List.empty

    for (obj <- objects){
      if(obj.contains("taxi")){
        var parts: Array[String] = obj.split(" ")
        taxi = taxi ::: List(parts(0))
      }
    }
    taxi = taxi.sorted
    return taxi
  }

  def passengersFromProblem (pathProblem : String, nameProblem : String):List[String]  = {
    var objects : List[String] = objectsFromProblem(pathProblem,nameProblem)
    var passengers : List[String] = List.empty

    for (obj <- objects){
      if(obj.contains("passenger")){
        var parts: Array[String] = obj.split(" ")
        passengers = passengers ::: List(parts(0))
      }
    }
    passengers = passengers.sorted
    return passengers
  }

  def locationsStrFromProblem (pathProblem : String, nameProblem : String):List[String]  = {
    var objects : List[String] = objectsFromProblem(pathProblem,nameProblem)
    var locations : List[String] = List.empty

    for (obj <- objects){
      if(obj.contains("location")){
        var parts: Array[String] = obj.split(" ")
        locations = locations ::: List(parts(0))
      }
    }
    locations = locations.sorted
    return locations
  }



  def startPlanningOnDocker (pathProblem: String, nameProblem:String, idContainer : String) : Int = {
    var pathProblemUnix : String = pathWindowsToPathUnix(pathProblem)
    val command = pathProblemUnix + "/lunchPlanningPOPF2Docker.sh " + idContainer + " " + pathProblemUnix + "/ domainS.pddl " + nameProblem + ""//  ^> out.txt")
    val os = sys.props("os.name").toLowerCase
    val commandToRun = os match {
      case x if x contains "windows" => "cmd " + "/C" + command
      case _ => command
    }

    val process : ProcessInfo = runCommandAndGetOutput(commandToRun)
    if(process.exitCode != 0){
      throw new RuntimeException("ERROR! Something went wrong with the planning setup or Planning on the Docker container")
    }
    process.exitCode
  }


  private def pathWindowsToPathUnix (pathWindows: String): String ={
    var pathUnix : String = pathWindows
    if(pathWindows.startsWith("C:\\")){
      pathUnix = pathUnix.replace("C:","")
    }
    pathUnix = pathUnix.replace("\\","/")
    pathUnix
  }

  private case class ProcessInfo(stdout: String, stderr: String, exitCode: Int)

  private def runCommandAndGetOutput(command: String): ProcessInfo = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder
    val status = command ! ProcessLogger(stdout append _, stderr append _)
    ProcessInfo(stdout.toString(), stderr.toString(), status)
  }



}
