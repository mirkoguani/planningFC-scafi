package it.unito.planningFC.taxiscenario

object Message {
  //taxi messages
  case class StartActionDriveT(actionDrive : Drive)
  case class EndActionDriveT(actionDrive : Drive)
  case class StartActionEnterT(actionEnter : Enter)
  case class EndActionEnterT(actionEnter : Enter)
  case class StartActionExitT(actionExit : Exit)
  case class EndActionExitT(actionExit : Exit)
  case class GetLocationT ()
  case class GetPassengerInT()
  case class SetLocationT(location:Location)
  case class SetPassengerInT(passenger:String)
  //passengers messages
  case class StartActionEnterP(actionEnter : Enter)
  case class EndActionEnterP(actionEnter : Enter)
  case class StartActionExitP(actionExit : Exit)
  case class EndActionExitP(actionExit : Exit)
  case class GetLocationP()
  case class GetInTaxiP()
  case class GetLocationGoalP()
  case class SetLocationP(location:Location)
  case class SetInTaxiP(taxi:String)
  case class SetLocationGoalP(location: Location)
}
