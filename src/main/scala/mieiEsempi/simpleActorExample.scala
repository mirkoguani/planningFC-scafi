import akka.actor.{Actor, ActorSystem, Props};

object simpleActorExample extends App{

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case s:String => println("String: " + s)
      case i:Int => println("Int: " + i)
    }
  } //end class Actor

  //ora nuovamente nell'App
  val system = ActorSystem("SimpleSystem")
  val actor = system.actorOf(Props[SimpleActor], "SimpleActor")

  actor ! "Hi there."
  actor ! 42

  system.terminate()

}
