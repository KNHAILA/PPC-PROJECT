package upmc.akka.leader

import akka.actor._

abstract class NodeStatus
case class Passive () extends NodeStatus
case class Candidate () extends NodeStatus
case class Dummy () extends NodeStatus
case class Waiting () extends NodeStatus
case class Leader () extends NodeStatus

abstract class LeaderAlgoMessage
case class Initiate () extends LeaderAlgoMessage
case class ALG (list:List[Int], nodeId:Int) extends LeaderAlgoMessage
case class AVS (list:List[Int], nodeId:Int) extends LeaderAlgoMessage
case class AVSRSP (list:List[Int], nodeId:Int) extends LeaderAlgoMessage

case class StartWithNodeList (list:List[Int])

class ElectionActor (val id:Int, val terminaux:List[Terminal]) extends Actor {

     val father = context.parent
     var nodesAlive:List[Int] = List(id)

     var candSucc:Int = -1
     var candPred:Int = -1
     var status:NodeStatus = new Passive ()

     //Nodes suiv (Les nodes connaissent leur successeur )
     var succ:Int = (id+1) % nodesAlive.size

     def receive = {

          // Initialisation
          case Start => {
               self ! Initiate
          }

          case StartWithNodeList (list) => {
               if (list.isEmpty) {
                    this.nodesAlive = this.nodesAlive:::List(id)
               }
               else {
                    this.nodesAlive = list
               }

               // Debut de l'algorithme d'election
               self ! Initiate
          }

          case Initiate => {
               //Set status to candidate
               this.status = new Candidate ()

               //On récupère la node voisine à init (son successeur)
               //succ = (init+1) % nodes.size

               //Send message to successor
               terminaux.foreach(n => {

                    if(n.id == succ){
                         val remote = context.actorSelection("akka.tcp://LeaderSystem" + n.id + "@" + n.ip + ":" + n.port + "/user/Node")
                         remote ! ALG(this.nodesAlive,id)
                    }
               })

          }

          case ALG (list, init) => {
               if (status == Passive ()){
                    this.status = new Dummy ()
                    //Send message to successor
                    terminaux.foreach(n => {

                         if(n.id == succ){
                              val remote = context.actorSelection("akka.tcp://LeaderSystem" + n.id + "@" + n.ip + ":" + n.port + "/user/Node")
                              remote ! ALG(this.nodesAlive,init)
                         }
                    })
               }

               if(status == Candidate ()){
                    this.candPred = init

                    if(id > init){
                        
                        if(this.candSucc == -1){
                             status = new Waiting ()

                             //Send message to init
                              terminaux.foreach(n => {

                                   if(n.id == init){
                                        val remote = context.actorSelection("akka.tcp://LeaderSystem" + n.id + "@" + n.ip + ":" + n.port + "/user/Node")
                                        remote ! AVS(this.nodesAlive,id)
                                   }
                              })

                        }
                        else {
                             //Send message to successor

                              terminaux.foreach(n => {

                                   if(n.id == candSucc){
                                        val remote = context.actorSelection("akka.tcp://LeaderSystem" + n.id + "@" + n.ip + ":" + n.port + "/user/Node")
                                        remote ! AVSRSP(this.nodesAlive,this.candPred)
                                   }
                              })
                              status = new Dummy ()
                        }
                    }
               }

               if( init == id)
                    status = new Leader ()
          }

          case AVS (list, j) => {
               if (status == Candidate ()) {
                    if(this.candPred == -1){
                         this.candPred = j
                    }
                    else {
                         //Send message to successor
                         terminaux.foreach(n => {

                              if(n.id == j){
                                   val remote = context.actorSelection("akka.tcp://LeaderSystem" + n.id + "@" + n.ip + ":" + n.port + "/user/Node")
                                   remote ! AVSRSP(this.nodesAlive,this.candPred)
                              }
                         })
                         status = new Dummy ()
                    }
               }
               else {
                    this.candSucc = j
               }
          }

          case AVSRSP (list, k) => {
               
               if(status == Waiting ()){

                    if(id == k){
                         status = new Leader ()
                    }
                    else {
                         this.candPred = k

                         if(candSucc == -1){

                              if(k < id){
                                   status = new Waiting ()
                                   //Send message to successor
                                   terminaux.foreach(n => {

                                        if(n.id == k){
                                             val remote = context.actorSelection("akka.tcp://LeaderSystem" + n.id + "@" + n.ip + ":" + n.port + "/user/Node")
                                             remote ! AVSRSP(this.nodesAlive,id)
                                        }
                                   })
                              }

                         }
                         else {
                              status = new Dummy ()
                              //Send message to successor
                              terminaux.foreach(n => {

                                   if(n.id == this.candSucc){
                                        val remote = context.actorSelection("akka.tcp://LeaderSystem" + n.id + "@" + n.ip + ":" + n.port + "/user/Node")
                                        remote ! AVSRSP(this.nodesAlive,k)
                                   }
                              })
                         }
                    }
               }

          }

     }

     

}
