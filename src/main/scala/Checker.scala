package upmc.akka.leader

import java.util
import java.util.Date

import akka.actor._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


abstract class Tick
case class CheckerTick () extends Tick

class CheckerActor (val id:Int, val terminaux:List[Terminal], electionActor:ActorRef) extends Actor {

     var time : Int = 200
     val father = context.parent

     var nodesAlive:List[Int] = List()
     var datesForChecking:List[Date] = List()
     var lastDate:Date = null

     var leader : Int = -1

     
    def receive = {

         // Initialisation
        case Start => {
             self ! CheckerTick

             // Je m'ajoute pour signaler aux autre nodes que je suis en vie (Initialisation)
             this.nodesAlive = this.nodesAlive:::List(id)

             //J'initialise le leader
             this.leader = 0

        }

        // A chaque fois qu'on recoit un Beat : on met a jour la liste des nodes
        case IsAlive (nodeId) => {
               
               if(!this.nodesAlive.contains(nodeId))
                    this.nodesAlive = this.nodesAlive:::List(nodeId)
               if(!this.nodesAlive.contains(id))
                    this.nodesAlive = this.nodesAlive:::List(id)

               print("Alives Nodes : "+this.nodesAlive.sorted+"\n")
               
          }

        case IsAliveLeader (nodeId) => {
             if(!this.nodesAlive.contains(nodeId))
                    this.nodesAlive = this.nodesAlive:::List(nodeId)
             println("Je suis le leader ("+nodeId+")")
            
        }

        // A chaque fois qu'on recoit un CheckerTick : on verifie qui est mort ou pas
        // Objectif : lancer l'election si le leader est mort
        case CheckerTick => {
          //Il faut checker qui est mort en plus du leader
          var tmp:List[Int] = List()
          
          context.system.scheduler.schedule(7 seconds, 7 seconds) {

               println("CHECKKKKK")
               this.nodesAlive = List()
                terminaux.foreach(n => {

                    val remote = context.actorSelection("akka.tcp://LeaderSystem" + n.id + "@" + n.ip + ":" + n.port + "/user/Node")
                    if(n.id == leader){
                              remote ! IsAliveLeader (n.id)
                    }
                    if( n.id != id){
                         remote ! IsAlive (id)
                   }
                  
                    
               })
               println(leader)
               if(!this.nodesAlive.contains(leader)){
                    print("OHHHH ------> "+this.nodesAlive)
                    println("Ancien Leader : "+leader+" Election en cours..")
               }
              
          }
      }
      
    }

}