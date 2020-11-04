package planningFC

import java.io.{File, FileWriter, FilenameFilter, IOException}
import java.util.Scanner

import akka.actor.{Actor, ActorRef, ActorSystem, Props}


class FEL {
  //Classe per eventi nella FEL (Future Event List)
   class event_action_FEL (name_ev: String, time_occur: Double) {
    var name_event: String = name_ev
    var time_occurrence: Double = time_occur
    }
    var actionsFEL : List [event_action_FEL] = List[event_action_FEL]()

    def addEventFEL (name_ev: String, time_occur: Double) = {
      val eventoAzione : event_action_FEL = new event_action_FEL(name_ev, time_occur)
      //println(eventoAzione.name_event)
      //println(eventoAzione.time_occurrence)

      actionsFEL = actionsFEL ::: List(eventoAzione)
      // ordina la lista per time_occurrence
      actionsFEL = actionsFEL.sortBy(_.time_occurrence)
    }

    def popEventFEL () : event_action_FEL = {
      var eventoPop: event_action_FEL = null
      //prima prendo l'evento
      if(actionsFEL.length > 0) {
        eventoPop = actionsFEL(0)
        actionsFEL = actionsFEL.drop(1) //il parametro indica quanti elementi rimuovere dalla testa
        println("eventoPop.name_event : " + eventoPop.name_event)
        println("eventoPop.time_occurrence : " + eventoPop.time_occurrence)
        return eventoPop
      } else {
        println("La Future Event List e' vuota, Simulazione terminata")
        return null
      }
    }

    def printEventsFEL () = {
      println("Future Event List :")
      for(eventAction <-actionsFEL)
      {
        println("At time " + eventAction.time_occurrence + "   " + eventAction.name_event)
      }
    }

    def FELSimultationFromPlan (planList:List[String]) = {
      actionsFEL = List.empty //reinizializza FEL
      addEventFEL("END_SIMULATION", Double.MaxValue)
      for (actionPlan <- planList){
        var parts: Array[String] = actionPlan.split(":");
        var timeStartAction: Double =  parts(0).toDouble //e' normale che lo sovrascriva intanto serve solamente il makespan finale
        parts = parts(1).split("\\[")
        var action : String = parts(0).trim()
        parts = parts(1).split("]")
        var durataAzione : Double = parts(0).toDouble
        addEventFEL("START: " + action, timeStartAction)
        addEventFEL("END : " + action, timeStartAction+durataAzione)
      }
    }

  def FELtoList() : List[String] = {
    var listFEL: List[String] = List.empty
    for(eventAction <-actionsFEL)
    {
      listFEL = listFEL ::: List("At time " + eventAction.time_occurrence + "   " + eventAction.name_event)
    }
    return listFEL
  }

}

object utilsPlanning {
  def planFromFile (pathPlan: String, nomePlan: String) : List[String] = {
    var planList : List[String] = List.empty
    val f: File = new File(pathPlan)
    // Populates the array with names of files and directories
    val pathnames: Array[String] = f.list();

    // For each pathname in the pathnames array
    /*for(i <- 0 to pathnames.length-1) {
      //println(pathnames(i))
    } */
    val filter: FilenameFilter = new FilenameFilter() {
      override def accept(dir: File, name: String): Boolean = {
        name.startsWith(nomePlan)
      }
    }

    val pathnamesstart: Array[String] = f.list(filter)
    /*for (i <- 0 to pathnamesstart.length - 1) {
      //println(pathnamesstart(i))
    } */

    /////////////////////
    // For each pathname in the pathnames array
    var maxLastPlan: Int = 0;
    var nomeFilePlan: String = nomePlan; //per default

    for (i <- 0 to pathnamesstart.length - 1) {
      // Print the names of files and directories
      println(pathnamesstart(i));
      val pathname :String = pathnamesstart(i)

      val parts:Array[String] = pathname.split("\\.");
      val numPlan :Int = Integer.parseInt(parts(1))
      if(numPlan > maxLastPlan){
        nomeFilePlan = parts(0)
        maxLastPlan = numPlan
      }
    }

    //try {
    //File myObj = new File(pathPlan + pathnamesstart[pathnamesstart.length-1]);
    val myObj: File = new File(pathPlan + "\\"+nomeFilePlan +"."+ maxLastPlan);
    System.out.println("Piano: " + pathPlan + nomePlan +"."+ maxLastPlan);
    val myReader :Scanner = new Scanner(myObj);
    var numRiga :Int = 0;
    while (myReader.hasNextLine()) {
      numRiga= numRiga +1
      val data :String = myReader.nextLine();
      //println(data);
      if(!data.startsWith(";")){
        planList = planList ::: List(data)
      }//if non inizia con ;
    } //end while
    myReader.close();

    return planList
  }

  /*def actorsFromPlan (planList:List[String]): List[String] ={
    var actorsList:List[String] = List.empty
    for (actionString <- planList){

    }
  } */

} //end utilsPlanning



object planningFcApp extends App {

  case class SendStartAction(action: String, replyTo: ActorRef)
  case class ResponseStartAction(result: String, sender:ActorRef)
  case class SendEndAction(action: String, replyTo: ActorRef)
  case class ResponseEndAction(result: String, sender:ActorRef)
  case class askLocation (replyTo: ActorRef)
  case class responseLocation(location:String, sender:ActorRef)


  class attore extends Actor {
    var location:String = "" //da mettere in pratica poi una init (intesa la init del problemS.pddl)

    override def receive: Receive = {

      case SendStartAction(action, replyTo) => {
        println("SendStartAction - Attore destinatario : " + self.path.name + " - Messaggio : " + action  + " - Attore mittente : "+ replyTo.path.name)
        replyTo ! ResponseStartAction("Sono l'attore " + self.path.name + " ti rispondo alla SendAction : ' " + action, self)
      }

      case ResponseStartAction(result,sender) => {
        //println("Sono l'attore Simulatore " + self.path.name + " ResponseStartAction e ho ricevuto la risposta : ' " + result + " ' dall'attore " + sender)
        println("ResponseStartAction - Attore destinatario : " + self.path.name + " - Messaggio : " + result  + " - Attore mittente : "+ sender.path.name)
      }

      case SendEndAction(action, replyTo) => {
        //println("Sono l'attore " + self.path.name + " SendEndAction e ho ricevuto l'azione : ' " + action + " ' dall'attore " + replyTo)
        println("SendEndAction - Attore destinatario : " + self.path.name + " - Messaggio : " + action  + " - Attore mittente : "+ replyTo.path.name)
        var actionParts :Array[String] = action.split("\\(")
        actionParts = actionParts(1).split("\\)")
        actionParts = actionParts(0).split(" ")
        var targetLocation : String = actionParts(actionParts.length-1)
        println("Cambio di stato Attore " + self.path.name + " --> Da location " + location + " A location " +targetLocation)
        //println("Sono l'attore " + self.path.name + "il mio stato e' appena cambiato --> da location " + location + " a location "+targetLocation)
        location = targetLocation
        replyTo ! ResponseStartAction("Sono l'attore " + self.path.name + " ti rispondo alla SendAction : ' " + action, self)
      }

      case ResponseEndAction(result,sender) => {
        //println("Sono l'attore Simulatore " + self.path.name + " ResponseEndAction e ho ricevuto la risposta : ' " + result + " ' dall'attore " + sender)
        println("ResponseEndAction - Attore destinatario : " + self.path.name + " - Messaggio : " + result  + " - Attore mittente : "+ sender.path.name)
      }


      case askLocation (replyTo: ActorRef) => {
        //println("Sono l'attore " + self.path.name + "il mio stato e' location : " + location)
        println("askLocation - Attore destinatario : " + self.path.name + " - Attore mittente : "+ replyTo.path.name)
        replyTo ! responseLocation(location, self)
      }

      case responseLocation (location:String, sender:ActorRef) => {
        //println("Sono l'attore Simulatore " + self.path.name + "  e la LOCATION dell'attore " + sender + " e' " + location)
        println("ResponseLocation - Attore destinatario : " + self.path.name + " - Messaggio : LOCATION dell'attore " + sender.path.name + " : " + location)

      }

    } //end override receive
  } //end attore



  val taxi : Array[String] = Array("t1","t2","t3");
  val passengers : Array[String] = Array("p1","p2","p3","p4","p5","p6","p7")
  //val pathAgenti : String = "C:\\Users\\Mirko\\Downloads\\tempo-sat-popf2\\actions_agents\\" //NB fai attenzione che esista!
  //val pathPlan :String = "C:\\Users\\Mirko\\Downloads\\tempo-sat-popf2 //NB fai attenzione che esista!
  val pathPlan :String = System.getProperty("user.dir")
  val nomePlan :String = "outp21";
  //val f: File = new File(pathPlan);

  val system = ActorSystem("SimulationActorSystem")
  val actorSimulator = system.actorOf(Props[attore](),"ActorSimulator")
  val actorsTaxi : Array[ActorRef] = new Array[ActorRef](taxi.length)
  val actorsPassengers : Array[ActorRef] = new Array[ActorRef](passengers.length)
  for(i <- 0 to taxi.length-1){
    val actorTaxi : ActorRef = system.actorOf(Props[attore](),"Actor"+taxi(i))
    actorsTaxi(i) = actorTaxi
  }
  for(i <- 0 to passengers.length-1){
    val actorPassenger : ActorRef = system.actorOf(Props[attore](),"Actor" + passengers(i))
    actorsPassengers(i) = actorPassenger
  }


//FEL
 var actionsFEL: FEL = new FEL


  var planList:List[String] = utilsPlanning.planFromFile(pathPlan,nomePlan)
  for (azioneString <- planList){
    println(azioneString)
  }
  println("--------end PlanFromFile")
  actionsFEL.FELSimultationFromPlan(planList)
  actionsFEL.printEventsFEL()

  println("---------end printFEL")

  var makespan :Double = 0.000
  var listFEL = actionsFEL.FELtoList()
  for (action <- listFEL){
    if (!action.contains("END_SIMULATION")) {
      var actionParts:Array[String] = action.split(" ")
      makespan = actionParts(2).toDouble
      println(action)
      for (i <- 0 to taxi.length - 1) { //ciclo su tutti i taxi per vedere  quali tra essi è coinvolto
        if (action.contains(taxi(i)) == true) {
          if(action.contains("START")) {
            actorsTaxi(i) ! SendStartAction(action, actorSimulator)
          }
          if(action.contains("END")) {
            actorsTaxi(i) ! SendEndAction(action, actorSimulator)
          }


        }
      }

      for (i <- 0 to passengers.length - 1) { //ciclo su tutti i passeggeri per vedere  quali tra essi è coinvolto
        if (action.contains(passengers(i)) == true) {
          if(action.contains("START")) {
            actorsPassengers(i) ! SendStartAction(action, actorSimulator)
          }
          if(action.contains("END")) {
            actorsPassengers(i) ! SendEndAction(action, actorSimulator)
          }


        }
      }
      //per ottenere l'ordine delle azioni usiamo una Sleep
      //println("ActorSystem attendo mezzo secondo ")
      Thread.sleep(500)

    } //end if !END_SIMULATION

    if(action.contains("END_SIMULATION")){
      //chiedo a tutti la loro location
      println("------")
      println("STATO FINALE SIMULAZIONE")
      println("Time: " + makespan)
      for (i <- 0 to taxi.length - 1) {
        actorsTaxi(i) ! askLocation(actorSimulator)
      }
      for (i <- 0 to passengers.length - 1) {
        actorsPassengers(i) ! askLocation(actorSimulator)
      }
    }

  }

  system.terminate() //termina ActorSystem
}