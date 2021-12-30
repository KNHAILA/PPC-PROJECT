package upmc.akka.leader

import akka.actor._

case class Start ()

class Node (val id:Int) extends Actor {

     // Les differents acteurs du systeme
     val displayActor = context.actorOf(Props[DisplayActor], name = "displayActor")

     def receive = {

          // Initialisation
          case Start => {
               displayActor ! Message ("Node " + this.id + " is created")        
          }

     }
}
