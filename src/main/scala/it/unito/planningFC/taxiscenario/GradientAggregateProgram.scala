package it.unito.planningFC.taxiscenario

import it.unibo.scafi.incarnations.BasicAbstractIncarnation

object MyIncarnationAP extends BasicAbstractIncarnation

import MyIncarnationAP._

class GradientAggregateProgram {
  val program: AggregateProgram = new classicGradient() //new hopGradient()   //Classic Gradient   programma aggregato

  def roundAggregateProgram (selfID : ID , exports : Map[ID,EXPORT], LSNS : Map[String,Any], NSNS : Map[NSNS,Map[ID,Any]]) : EXPORT = {
    val ctx = factory.context(selfID,exports, LSNS, NSNS)
    return program.round(ctx)
  }

}


class hopGradient extends AggregateProgram { //hopGradient  programma aggregato
  def locationSource = sense[Boolean]("locationSource")
  def boolToInt(b: Boolean) = mux(b){1}{0}
  def nbrRange = nbrvar[Double]("nbrRange")

  override def main() = rep(Double.PositiveInfinity){
    hops => { mux(locationSource) { 0.0 } { 1 + minHoodPlus(nbr{ hops }) } }
  }
}

class classicGradient extends AggregateProgram {

  def locationSource = sense[Boolean]("locationSource")
  def typeDevice : String = sense[String]("typeDevice")
  def boolToInt(b: Boolean) = mux(b){1}{0}
  def nbrRange = nbrvar[Double]("nbrRange")

  override def main() = {
    rep(Double.MaxValue) {
    distance =>
      mux(locationSource) {
        0.0
      } {
        minHoodPlus {
          nbr {
            distance
          } + nbrvar[Double](NBR_RANGE)
        }
      }
    }
  }
}