package it.unito.planningFC.taxiscenario


class Drive (action: String) {
  var actionType: String = ""
  var taxi: String = ""
  var locationFrom: String = ""
  var locationTo: String = ""

  //parsing the action
  private var actionParsed: String = action //default
  if (action.contains("drive")) {
    actionType = "drive"
    var parts: Array[String] = action.split("drive")
    //if format is drive t1 e h3
    actionParsed = parts(1).trim
    //if format is :  At time 62.008   START: (drive t1 e h3)
    if (parts(1).contains(")")) {
      parts = parts(1).split("\\)")
      actionParsed = parts(0).trim
    }
    //so we get from the drive action:   t1 e h3
    parts = actionParsed.split(" ")
    taxi = parts(0)
    locationFrom = parts(1)
    locationTo = parts(2)
  }
}

class Enter (action: String) {
  var actionType: String = ""
  var passenger: String = ""
  var taxi: String = ""
  var location: String = ""

  //parsing the action
  private var actionParsed: String = action //default
  if (action.contains("enter")) {
    actionType = "enter"
    var parts: Array[String] = action.split("enter")
    //if format is enter p6 t1 h1
    actionParsed = parts(1).trim
    //if format is :  At time 51.006   START: (enter p6 t1 h1)
    if (parts(1).contains(")")) {
      parts = parts(1).split("\\)")
      actionParsed = parts(0).trim
    }
    //so we get from the enter action:   p6 t1 h1
    parts = actionParsed.split(" ")
    passenger = parts(0)
    taxi = parts(1)
    location = parts(2)
  }
}

class Exit (action: String) {
  var actionType: String = ""
  var passenger: String = ""
  var taxi: String = ""
  var location: String = ""

  //parsing the action
  private var actionParsed: String = action //default
  if (action.contains("exit")) {
    actionType = "exit"
    var parts: Array[String] = action.split("exit")
    //if format is exit p6 t1 h3
    actionParsed = parts(1).trim
    //if format is :  At time 72.009   START: (exit p6 t1 h3)
    if (parts(1).contains(")")) {
      parts = parts(1).split("\\)")
      actionParsed = parts(0).trim
    }
    //so we get from the enter action:   p6 t1 h3
    parts = actionParsed.split(" ")
    passenger = parts(0)
    taxi = parts(1)
    location = parts(2)
  }
}