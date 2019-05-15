import Automata._
import akka.actor.{Actor, ActorSystem, Props}

object Main {
  val system = ActorSystem("automata")
  val automata = system.actorOf(Props[Automata])
  def read(sentence: Seq[Alphabet]) =
    sentence.foreach(word => automata ! word)
  def main(args: Array[String]): Unit = {
    read(Seq(Subject/*she*/, Verb/*likes*/, Object/*potato*/, End))// 0
    read(Seq(Subject/*she*/, FrequencyAdverb /*often*/,Verb/*goes*/, Object/*to cinema*/, End))//0
    read(Seq(Subject/*she*/, FrequencyAdverb /*often*/,Verb/*goes*/, Object/*to cinema*/, Time /*every Saturday*/, End))//0
    read(Seq(Subject/*she*/, FrequencyAdverb /*often*/,Verb/*goes*/, Time /*often*/, Object/*to cinema*/, End))//0
    read(Seq(Verb/*likes*/, Object/*potato*/, End))//1
    read(Seq(Subject/*she*/, Verb/*goes*/, FrequencyAdverb /*sometimes*/,Object/*to home*/, End))//1
    read(Seq(Subject/*she*/, Verb/*goes*/, FrequencyAdverb /*sometimes*/,Object/*to home*/, Subject/*he*/, End))//1
  }
}

class Automata extends Actor {
  val success = true
  val failure = false
  sealed trait State{
    val name: String
    def processFunction: Receive
    def withFailure(receive: Receive): Receive = {
      receive.orElse{
        case _ => sys.exit(1)
      }
    }
    def goToState(newState: State): Unit = {
      println(s"go to ${newState.name}")
      context.become(withFailure(newState.processFunction))
    }
  }
  case object Q1 extends State {
    val name: String = "Q1"

    def processFunction: Receive = {
      case Verb => goToState(Q2)
      case FrequencyAdverb => goToState(Q3)
    }
  }
  case object Q2 extends State {
    val name: String = "Q2"

    def processFunction: Receive = {
      case Time => goToState(Q4)
      case Object => goToState(Q5)
    }
  }
  case object Q3 extends State {
    val name: String = "Q3"

    def processFunction: Receive = {
      case Verb => goToState(Q2)
    }
  }
  case object Q4 extends State {
    val name: String = "Q4"

    def processFunction: Receive = {
      case Object => goToState(Q5)
      case Time => goToState(this)
    }
  }
  case object Q5 extends State {
    val name: String = "Q5"

    def processFunction: Receive = {
      case Time => goToState(Q6)
      case End => sys.exit(0)
    }
  }
  case object Q6 extends State {
    val name: String = "Q6"

    def processFunction: Receive = {
      case End => sys.exit(0)
    }
  }
  override def receive: Receive = {
    case Subject => Q1.goToState(Q1)
    case _ => sys.exit(1)
  }
}

object Automata{
  trait Alphabet
  case object Verb extends Alphabet
  case object Object extends Alphabet
  case object Subject extends Alphabet
  case object Time extends Alphabet
  case object FrequencyAdverb extends Alphabet
  case object End extends Alphabet
}