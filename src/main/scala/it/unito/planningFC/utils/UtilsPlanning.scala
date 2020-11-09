package it.unito.planningFC.utils

import java.io.{File, FilenameFilter}
import java.util.Scanner

import org.apache.log4j.Logger

object UtilsPlanning {

  val log = Logger.getLogger(this.getClass.getName)

  def planFromFile (pathPlan: String, namePlan: String) : List[String] = {
    var planList : List[String] = List.empty
    val f: File = new File(pathPlan)

    val filter: FilenameFilter = new FilenameFilter() {
      override def accept(dir: File, name: String): Boolean = {
        name.startsWith(namePlan)
      }
    }

    val pathnames: Array[String] = f.list(filter)
    var maxLastPlan: Int = 0;
    var nameFilePlan: String = namePlan; //default

    for (i <- pathnames.indices) {
      val pathname :String = pathnames(i)

      val parts:Array[String] = pathname.split("\\.");
      val numPlan :Int = Integer.parseInt(parts(1))
      if(numPlan > maxLastPlan){
        nameFilePlan = parts(0)
        maxLastPlan = numPlan
      }
    }

    val myObj: File = new File(pathPlan + "\\"+nameFilePlan +"."+ maxLastPlan);
    log.debug("Plan: " +pathPlan + "\\"+nameFilePlan +"."+ maxLastPlan);
    val myReader :Scanner = new Scanner(myObj);
    while (myReader.hasNextLine) {
      val data :String = myReader.nextLine();
      if(!data.startsWith(";")){ //ignores some comments row at the beginning of the Plan file
        planList = planList ::: List(data)
      }
    }
    myReader.close();

    return planList
  }
}
