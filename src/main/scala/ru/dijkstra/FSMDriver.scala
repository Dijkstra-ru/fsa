package ru.dijkstra
import ru.dijkstra.ast._

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
     sb.append(st.name).append(": ").append("\n")
     st.tableRow map {
        a => sb.append("   '").append(a.symbol).append("' ").append(a.newState).append("\n")
     }
     sb.append("\n")
   }
   }
   sb
  }
}
