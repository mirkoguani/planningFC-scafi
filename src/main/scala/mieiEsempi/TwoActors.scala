package mieiEsempi

import akka.actor.{Actor, ActorRef, ActorSystem, Props}


object TwoActors extends App {
  case class InviaMessaggio(msg:String, sender:ActorRef)

  class attore extends Actor {

    override def receive: Receive = {
      case InviaMessaggio(msg, sender) => {
        println("Sono l'attore " + self.path.name + " e ho ricevuto il messaggio : ' " + msg + " ' dall'attore " + sender)
      }
    }
  }


  val system = ActorSystem("TwoActorSystem")
  val actor1 = system.actorOf(Props[attore](),"actor1")
  val actor2 = system.actorOf(Props[attore](),"actor2")

  actor1 ! InviaMessaggio("Invio questo messaggio all' attore2", actor2)
  actor2 ! InviaMessaggio("Invio questo messaggio all' attore1", actor1)

  system.terminate()

}