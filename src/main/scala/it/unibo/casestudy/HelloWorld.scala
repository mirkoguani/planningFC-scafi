package it.unibo.casestudy

import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.interfaces.Node
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class HelloWorld extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with Gradients {
  override def main(): Any = {
    // Access to node state through "molecule"
    val x = if(node.has("prova")) node.get[Int]("prova") else 1
    // An aggregate operation
    val g = classicGradient(mid==100)
    /*if(mid==100){
      //val coordinates : List[Double]= alchemistEnvironment.getPosition(alchemistEnvironment.getNodeByID(mid)).getCartesianCoordinates.toList
     // println("sono il 100 e le mie coordinate sono " + coordi)
      val nodo = alchemistEnvironment.getNodeByID(100)
    } */
    // Write access to node state
    node.put("g", g)
    // Return value of the program
    g
  }
}