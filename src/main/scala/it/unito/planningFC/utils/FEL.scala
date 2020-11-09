package it.unito.planningFC.utils

import org.apache.log4j.Logger

class FEL {
  //class for events of the FEL (Future Event List)
  class EventActionFEL (nameEv: String, timeOccur: Double) {
    var nameEvent: String = nameEv
    var timeOccurrence: Double = timeOccur
  }
  var actionsFEL : List [EventActionFEL] = List[EventActionFEL]()

  val log = Logger.getLogger(this.getClass.getName)

  def addEventFEL (nameEv: String, timeOccur: Double): Unit = {
    val eventAction : EventActionFEL = new EventActionFEL(nameEv, timeOccur)

    actionsFEL = actionsFEL ::: List(eventAction)
    // List sorted by timeOccurrence
    actionsFEL = actionsFEL.sortBy(_.timeOccurrence)
  }

  def popEventFEL () : EventActionFEL = {
    var eventPop: EventActionFEL = null
    //first take the event at the head of the FEL
    if(actionsFEL.nonEmpty) {
      eventPop = actionsFEL.head
      actionsFEL = actionsFEL.drop(1) //the parameter indicates how many elements to remove from the FEL's head
      //log.debug("eventPop.nameEvent : " + eventPop.nameEvent)
      //log.debug("eventPop.timeOccurrence : " + eventPop.timeOccurrence)
      return eventPop
    } else {
      //log.debug("Future Event List is empty, Simulation finished")
      return null
    }
  }

  def printEventsFEL (): Unit = {
    log.info("Future Event List :")
    for(eventAction <-actionsFEL)
    {
      log.info("At time " + eventAction.timeOccurrence + "   " + eventAction.nameEvent)
    }
  }

  def FELSimultationFromPlan (planList:List[String]): Unit = {
    actionsFEL = List.empty //reinitialize FEL
    addEventFEL("END_SIMULATION", Double.MaxValue)
    for (actionPlan <- planList){
      var parts: Array[String] = actionPlan.split(":");
      var timeStartAction: Double =  parts(0).toDouble
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
      listFEL = listFEL ::: List("At time " + eventAction.timeOccurrence + "   " + eventAction.nameEvent)
    }
    return listFEL
  }

}