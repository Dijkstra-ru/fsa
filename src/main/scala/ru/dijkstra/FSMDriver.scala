package ru.dijkstra
import ru.dijkstra.ast._
import collection.{Set, mutable}
import collection.mutable.ListBuffer
import collection.immutable.TreeMap

/**
 * FSM interpreter
 * @param driven FSM to operate
 */

class FSMDriver(driven : FSM) {
  var states = driven states
  var state = (states head) name
  var allowed = (states head) allow

  var vm = new VirtualMachine
  driven.before match { case Some(bl) => vm.run(bl) case None => }
  def next(symbol: String) = states find { _.name == state } match {
    case Some(x) => {
      x.tableRow find { _.symbol == symbol } match {
        case Some(node) => {
          state = node.newState
          states find { _.name == state } match {
              case Some(x) => allowed = x.allow
              case None => throw new Exception("Tried to change into unknown state " + state)
            }
          }
          node.semantics match {
            case Some(block) => vm.run(block)
            case None =>
          }
        case None => throw new Exception("State " + state + " has no instructions on symbol " + symbol)
      }
    }
    case None => throw new Exception("Unknown State: " + state)
  }
  def finalActions = driven.after match { case Some(bl) => vm.run(bl) case None => }
  def unloadFSM : StringBuilder = {
    var sb = new StringBuilder()
   states map { st => {
     st.allow match {
       case true => sb.append("accept ")
       case false => sb.append("decline ")
     }
     sb.append(st.name).append(": ").append("\n")
     st.tableRow map {
        a => sb.append("   '").append(a.symbol).append("' ").append(a.newState).append("\n")
     }
     sb.append("\n")
   }
   }
   sb
  }
  def inputAlphabet =
    ((states flatMap { _.tableRow map { _.symbol } } toSet) toList) sortWith { _.compareTo(_) < 0 }


  def normalForm {
    var il = states partition(_.allow)
    var groups = new mutable.HashMap[State, Int]()
    def stateByName(name: String) = states find( _.name == name ) get
    def out(sym: String, state: State) = (state tableRow) find( _.symbol == sym ) match {
      case Some(x) => { groups get(stateByName(x.newState)) get }
      case None => -1
    }
    def buildGroups = {
      val maxIndex = groups maxBy( _._2 ) _2
      var result = new ListBuffer[List[State]]()
      for (i <- 0 until maxIndex + 1) {              // = groups clone()
        result += groups.filter(_._2 == i).keys.toList
      }
      result toList
    }
    (il _1) map { state => groups += (state -> 0) }
    (il _2) map { state => groups += (state -> 1) }
    var newIndex = 2
    var changesMade = false
    do {
     changesMade = false
     inputAlphabet map {
       sym => {
          buildGroups map {
            group => {
              val firstout = out(sym, group head)
              if ((group forall(a => out(sym, a) == firstout)) == false) {
                val part = group partition(out(sym, _) == firstout)
                (part _1) map { state => groups += (state -> firstout) }
                (part _1) map { state => groups += (state -> newIndex) }
                newIndex += 1
                changesMade = true
              }
            }
          }
        }
      }
    } while (changesMade)
    val newNames = new mutable.HashMap[State, String]();
    //
    //println()
    //groups map { gr => println(gr._1.name + " " + gr._2) }
    //println()
    //
    groups map { gr => newNames += (gr._1 -> ("optimized_" + gr._2)) }
    val newStates = states map {
      state => {
        val newName = newNames.get(state).get
        val newAllow = state.allow
        val newRow = state.tableRow map {
          case TableNode(sym, state, _) => TableNode(sym, newNames.get(stateByName(state)).get, None)
        }
        State(newAllow, newName, newRow)
      }
    }
    newStates sortWith { (a, b) => a.name == (newNames.head._2) }
    states = (newStates toSet) toList
  }
}
