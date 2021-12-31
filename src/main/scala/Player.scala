package upmc.akka.leader

import math._

import javax.sound.midi._
import javax.sound.midi.ShortMessage._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import upmc.akka.ppc.DataBaseActor.Measure
import upmc.akka.ppc.DataBaseActor.Note
import upmc.akka.ppc.DataBaseActor.Chord
import upmc.akka.ppc.DataBaseActor.ObjetMusical
import scala.concurrent.duration._

import akka.actor.{Props, Actor, ActorRef, ActorSystem}


object PlayerActor {
  case class MidiNote (pitch:Int, vel:Int, dur:Int, at:Int) 
  case class PlayMeasure(mesure : Measure)
  val info = MidiSystem.getMidiDeviceInfo().filter(_.getName == "Gervill").headOption
  // or "SimpleSynth virtual input" or "Gervill"
  val device = info.map(MidiSystem.getMidiDevice).getOrElse {
    println("[ERROR] Could not find Gervill synthesizer.")
    sys.exit(1)
}

val rcvr = device.getReceiver()

  /////////////////////////////////////////////////
  def note_on (pitch:Int, vel:Int, chan:Int): Unit = {
      val msg = new ShortMessage
      msg.setMessage(NOTE_ON, chan, pitch, vel)
      rcvr.send(msg, -1)
  }
  
  def note_off (pitch:Int, chan:Int): Unit = {
      val msg = new ShortMessage
      msg.setMessage(NOTE_ON, chan, pitch, 0)
      rcvr.send(msg, -1)
  }
 
}

//////////////////////////////////////////////////


  
 

class PlayerActor () extends Actor{
  import DataBaseActor._
  import PlayerActor._
  device.open()

  def send_a_note (p:Int, d:Int, v:Int, at:Int): Unit = {
   self ! MidiNote(p,v,d,at)
  }

  def play (obj:ObjetMusical, at:Int): Unit =
  obj match {
    case Note(p,d,v) => send_a_note (p,d,v, at)
    case Chord (k,l) => {var date = k
                        l.foreach(n=>{play(n,date)})}
    case Measure (l) => l.foreach(n=>play(n,at))
  }

  def receive = {
    case PlayMeasure (l) => {
      play(l, 0)
    }
    case MidiNote(p,v, d, at) => {
      context.system.scheduler.scheduleOnce ((at) milliseconds) (note_on (p, v, 10))
      context.system.scheduler.scheduleOnce ((at+d) milliseconds) (note_off (p, 10))
    }
  }
}

