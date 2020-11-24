package it.unito.planningFC

import it.unibo.scafi.incarnations.BasicAbstractIncarnation

object MyIncarnationB extends BasicAbstractIncarnation

import MyIncarnationB._

class hopGradient extends AggregateProgram { //hopGradient  programma aggregato
  def sense1 = sense[Boolean]("sens1")
  def sense2 = sense[Boolean]("sens2")
  def sense3 = sense[Boolean]("sens3")
  def boolToInt(b: Boolean) = mux(b){1}{0}
  def nbrRange = nbrvar[Double]("nbrRange")*100

  override def main() = rep(Double.PositiveInfinity){
    hops => { mux(sense1) { 0.0 } { 1 + minHood(nbr{ hops }) } }
  }
}


object TestScafiOnSingleDevice extends App {


  var neighborhoodByDevice : Map[ID, Set[ID]] = Map()
  var exportsByDevice : Map[ID,Map[ID,EXPORT]] = Map() //each device has the exports of neighboring devices
  var LSNSByDevice : Map[ID,Map[String,Any]] = Map()
  var NSNSByDevice : Map[ID,Map[NSNS,Map[ID,Any]]] = Map()


  val program: AggregateProgram = new hopGradient()   //Classic Gradient   programma aggregato

  //setup
  //neighborhoodByDevice + ()  //ms + (k -> v)

  /*----------------------------------------------------------------------------- */
  //Device 1
  //i'm the actor Taxi t1 e.g.
    neighborhoodByDevice += (1 -> Set())  //ms + (k -> v)
    exportsByDevice += (1 -> Map())
    LSNSByDevice += (1 -> Map("sens1" -> true))
    NSNSByDevice += (1 -> Map("nbrRange" -> Map(1 -> 0.0)))


  val ctx1 = factory.context( //Contesto (Device1) :

    selfId = 1,
    exports = exportsByDevice(1), /* in generale li otterrò comunicando con altri nodi */
    lsens = LSNSByDevice(1) , //Map("sens1" -> true),
    nbsens = NSNSByDevice(1) ) //Map("nbrRange" -> Map(1 -> 0.0)))

  for ((k,v) <- neighborhoodByDevice) printf("key: %s, value: %s\n", k, v)

  var export1 : EXPORT = program.round(ctx1) //Export (round 1) del device 1
  println("Export device 1 (round 1): " + export1)

  //calcolato l'export del device 1 poi va inviato agli altri dal simulatore quindi presumo tu debba inviare a chi ce l'ha nel vicinato
  //gli invii l'export.


  //Contesto (round 2) del device 1

  neighborhoodByDevice += (1 -> Set())  //ms + (k -> v)
  exportsByDevice += (1 -> Map(1 -> export1))
  LSNSByDevice += (1 -> Map("sens1" -> true))
  NSNSByDevice += (1 -> Map("nbrRange" -> Map(1 -> 0.0)))
  for ((k,v) <- neighborhoodByDevice) printf("key: %s, value: %s\n", k, v)


  //var ctx12 = factory.context(1, Map(1 -> export1), Map("sens1" -> true), Map("nbrRange" -> Map(1 -> 0.0)))
  var ctx12 = factory.context(1, exportsByDevice(1), LSNSByDevice(1), NSNSByDevice(1))
  export1 = program.round(ctx12) //Export (round 2)  del device 1




  /* ------------------------------------------------------------------------------- */
  //Device 2

  neighborhoodByDevice += (2 -> Set(1))  //ms + (k -> v)
  neighborhoodByDevice += (1 -> Set(2)) //credo
  exportsByDevice += (2 -> Map(1 -> export1))
  LSNSByDevice += (2 -> Map("sens1" -> false))
  NSNSByDevice += (2 -> Map("nbrRange" -> Map(1 -> 1.5, 2 -> 0.0)))


  /*val ctx2 = factory.context(   //Contesto (round 1) del Device2 :
    selfId = 2,
    exports = Map(1 -> export1),   //a disposizione ha già l'export1 (che era però già data dal Secondo Round del device 1)
    lsens = Map("sens1" -> false),
    nbsens = Map("nbrRange" -> Map(1 -> 1.5, 2 -> 0.0))) //sono distante 1.5 dal Device 1, e 0.0 da me stesso */
  var ctx2 = factory.context(2, exportsByDevice(2), LSNSByDevice(2), NSNSByDevice(2))

  var export2 = program.round(ctx2)
  println("Export device 1 (round 2): " + export1)
  println("Export device 2 (round 1) : " + export2)

  var ctx22 = factory.context(2, Map(1 -> export1, 2 -> export2), Map("sens1" -> false), Map("nbrRange" -> Map(1 -> 1.5, 2 -> 0.0)))
  export2 = program.round(ctx22)
  println("Export device 2 (round 2) : " + export2)


  /*-----------------------------------------------------------------------------------------*/
  //Device 3
  var ctx3 = factory.context(   //Contesto (round 1) del Device3 :
    selfId = 3,
    exports = Map(2 -> export2),   //a disposizione ha già l'export2 (che è però già data dal Secondo Round del device 2)
    lsens = Map("sens1" -> false),
    nbsens = Map("nbrRange" -> Map(2 -> 4.0 , 3 -> 0.0))) //sono distante 4 dal Device 2, e 0.0 da me stesso

  var export3 = program.round(ctx3)


  println("Export device 1 (round 2): " + export1)
  println("Export device 2 (round 2) : " + export2)
  println("Export device 3 (round 1) : " + export3)



  for (i <- 3 to 13){

    neighborhoodByDevice += (1 -> Set(1,2))  //ms + (k -> v)
    neighborhoodByDevice += (2 -> Set(2,1,3)) //credo
    neighborhoodByDevice += (3 -> Set(2,3))
    exportsByDevice += (1 -> Map(1 -> export1, 2 -> export2))
    exportsByDevice += (2 -> Map(1 -> export1, 2 -> export2))
    exportsByDevice += (3 ->Map(2 -> export2, 3 -> export3))
    LSNSByDevice += (1 -> Map("sens1" -> true))
    LSNSByDevice += (2 -> Map("sens1" -> false))
    LSNSByDevice += (3 -> Map("sens1" -> false))
    NSNSByDevice += (1 -> Map("nbrRange" -> Map(1 -> 0.0, 2-> 1.5)))
    NSNSByDevice += (2 -> Map("nbrRange" -> Map(1 -> 1.5, 2 -> 0.0)))
    NSNSByDevice += (3 -> Map("nbrRange" -> Map(2 -> 4.0, 3 -> 0.0)))


    ctx12 = factory.context(1, exportsByDevice(1), LSNSByDevice(1), NSNSByDevice(1))
    ctx22 = factory.context(2, exportsByDevice(2), LSNSByDevice(2), NSNSByDevice(2))
    ctx3 = factory.context(3, exportsByDevice(3), LSNSByDevice(3), NSNSByDevice(3))
    export1 = program.round(ctx12) //Export (round 2)  del device 1
    export2 = program.round(ctx22)
    export3 = program.round(ctx3)

    println("Export device 1 (round "+i+") : " + export1)
    println("Export device 2 (round "+i+") : " + export2)
    println("Export device 3 (round "+(i-1)+") : " + export3)

  }


}