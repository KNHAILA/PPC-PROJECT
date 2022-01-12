package upmc.akka.leader

import akka.actor._

case class Start ()

sealed trait SyncMessage
case class Sync (nodes:List[Int]) extends SyncMessage
case class SyncForOneNode (nodeId:Int, nodes:List[Int]) extends SyncMessage

sealed trait AliveMessage
case class IsAlive (id:Int) extends AliveMessage
case class IsAliveLeader (id:Int) extends AliveMessage

// Ajout de ma part
// abstract class LeaderAlgoMessage
// case class ALG (list:List[Int], nodeId:Int) extends LeaderAlgoMessage
// case class AVS (list:List[Int], nodeId:Int) extends LeaderAlgoMessage
// case class AVSRSP (list:List[Int], nodeId:Int) extends LeaderAlgoMessage

class Node (val id:Int, val terminaux:List[Terminal]) extends Actor {
     
     // Les differents acteurs du systeme
     val electionActor = context.actorOf(Props(new ElectionActor(this.id, terminaux)), name = "electionActor")
     val checkerActor = context.actorOf(Props(new CheckerActor(this.id, terminaux, electionActor)), name = "checkerActor")
     val beatActor = context.actorOf(Props(new BeatActor(this.id)), name = "beatActor")
     val displayActor = context.actorOf(Props[DisplayActor], name = "displayActor")

     var allNodes:List[ActorSelection] = List()

     def receive = {

          // Initialisation
          case Start => {
               displayActor ! Message ("Node " + this.id + " is created")
               checkerActor ! Start
               beatActor ! Start


               // Initilisation des autres remote, pour communiquer avec eux
               terminaux.foreach(n => {
                    if (n.id != id) {
                         val remote = context.actorSelection("akka.tcp://LeaderSystem" + n.id + "@" + n.ip + ":" + n.port + "/user/Node")
                         // Mise a jour de la liste des nodes
                         this.allNodes = this.allNodes:::List(remote)

                    }
                   
               })

          }

          case Sync (listNode) => {

          }
          

          // Envoi de messages (format texte)
          case Message (content) => {
               displayActor ! Message (content)
          }

          case BeatLeader (nodeId) => 

          case Beat (nodeId) => {
               
               //checkerActor ! IsAlive (nodeId)
               
               terminaux.foreach(n => {
                   
                   
                         val remote = context.actorSelection("akka.tcp://LeaderSystem" + n.id + "@" + n.ip + ":" + n.port + "/user/Node")
                         remote ! IsAlive (nodeId)
                   
                    
               })
               
           }

          // Messages venant des autres nodes : pour nous dire qui est encore en vie ou mort
          case IsAlive (id) => {
               checkerActor ! IsAlive (id)
          }

          case IsAliveLeader (id) => {
               checkerActor ! IsAliveLeader (id)
          }

          // Message indiquant que le leader a change
          case LeaderChanged (nodeId) =>  {

          }

          case ALG(nodes:List[Int],init:Int) => 
          {
               
               electionActor ! ALG (nodes, init)
          }

          case AVS(nodes:List[Int],init:Int) =>
          {
               electionActor ! AVS (nodes, init)
          }

          

     }

}
