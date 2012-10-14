package ru.dijkstra.fsa
import scala.util.parsing.combinator.RegexParsers
import collection.mutable
import javax.swing.JFileChooser
import javax.swing.filechooser.FileNameExtensionFilter
import java.io.{File, FileReader, BufferedReader}
import java.util.Scanner

// Arithmetic ops
abstract trait Ar
case object Add extends Ar
case object Sub extends Ar
case object Mul extends Ar

// Vm commands
abstract trait Statement
case class Ar_Op_Const(id: String, const: Int, op: Ar) extends Statement
case class Ar_Op_Var(op1: String, op2: String, op: Ar) extends Statement
case class Print_Const(what: String) extends Statement
case class Print_Var(what: String) extends Statement
case class New_Var(what: String) extends Statement
case class Call(name: String) extends Statement
case class InvertDigits(name: String) extends Statement
case class Assign(name: String, value: Int) extends Statement
case object Failure_t extends Statement
case class Block(inner: List[Statement])
case class Proc(name: String, prog: Block) extends Statement

// Finite state machine representation
case class TableNode(symbol: String, newState: String, semantics: Option[Block])
case class State(name: String, tableRow: List[TableNode])
case class FSM(before: Option[Block], states:List[State], after:Option[Block])

// Parser
class FiniteAutoParser extends RegexParsers {
  override type Elem = Char
  // Inner language
  def identifier  = """[_\p{L}][_\p{L}\p{Nd}]*""".r
  def integer     = """-{0,1}(0|[1-9]\d*)""".r ^^ { _.toInt }
  def string      = "'.+'".r
  def invert      = "inv" ~> identifier ^^ { InvertDigits(_) }
  def call        = "call" ~> identifier ^^ { Call(_) }
  def failure     = "failure" ^^ { _ => Failure_t }
  def newvar      = "var" ~> identifier ^^ { New_Var(_) }
  def print       = "print" ~> string ^^ { Print_Const(_) }
  def assign      = (identifier <~ "=") ~ integer ^^ { case a~b => Assign(a, b) }
  def output      = "out" ~> identifier ^^ { Print_Var(_) }
  def proc        = "proc" ~> identifier ~ block  ^^ { case a~b => Proc(a,b) }
  def op_sign     = ("*"|"+"|"-") ^^
    { case "*" => Mul case "-" => Sub case "+" => Add }
  def ar_op = identifier ~ (op_sign <~ "=") ~ (integer | identifier) ^^
    {
      case a~op_sign~(b : Int) => Ar_Op_Const(a, b, op_sign)
      case a~op_sign~(b : String) => Ar_Op_Var(a, b, op_sign)
    }
  def ops : Parser[List[Statement]] =
    ((newvar | ar_op | print | output | failure | call | proc | assign | invert) <~ ";")*
  def block = "{" ~> ops <~ "}" ^^ { l => Block(l) }
  // FSM
  def tablenode   = ("'" ~> ".".r <~ "'") ~ identifier ~ opt(block) ^^ { case a~b~c => TableNode(a, b, c) }
  def state       = (identifier <~ ":") ~ (tablenode*) ^^ { case a~b => State(a, b) }
  def fsm         = opt(block) ~ (state*) ~ opt(block) ^^ { case a~b~c => FSM(a, b, c)  }
}

class VirtualMachine {
  var memory = new mutable.HashMap[String, Int]()
  var proc_memory = new mutable.HashMap[String, Block]()
  def run(prog : Block) : Unit = prog match {case Block(a) => a map { run(_) }}
  def run(oper : Statement ) : Unit = oper match {
    case New_Var(a) => memory += (a -> 0)
    case Ar_Op_Const(a, b, op) =>
       memory get(a) match {
         case Some(i) => memory += (op match {
           case Mul => (a, i * b)
           case Add => (a, i + b)
           case Sub => (a, i - b)
         })
         case None => throw new Exception("Use of undeclared variable: " + a)
       }
    case Ar_Op_Var(a, b, op) => memory get(b) match {
      case Some(y) => memory get(a) match {
        case Some(x) =>  memory += (op match {
          case Mul => (a, x * y)
          case Add => (a, x + y)
          case Sub => (a, x - y)
        })
        case None => throw new Exception("Use of undeclared variable: " + b)
      }
      case None => throw new Exception("Use of undeclared variable: " + a)
    }
    case Print_Var(a) => memory get(a) match {
      case Some(a) => println(a)
      case None => throw new Exception("Use of undeclared variable: " + a)
    }
    case Print_Const(a) => println(a)
    case Failure_t => throw new Exception("String rejected")
    case Proc(name, prog) => proc_memory += (name -> prog)
    case Call(name) => proc_memory get(name) match {
        case Some(a) =>  run(a)
        case None => throw new Exception("Use of undeclared procedure: " + name)
    }
    case Assign(a, b) => memory += (a -> b)
    case InvertDigits(a) => memory get(a) match {
      case Some(b) => memory += (a -> b.toString().reverse.toInt)
      case None => throw new Exception("Use of undeclared variable: " + a)
    }
  }
}

class FSMDriver(driven : FSM) {
  var states = driven states
  var state = (states head) name

  var vm = new VirtualMachine
  driven.before match { case Some(bl) => vm.run(bl) case None => }
  def next(symbol: String) = states find { _.name == state } match {
    case Some(x) => {
      x.tableRow find { _.symbol == symbol } match {
        case Some(node) => {
          state = node.newState
          node.semantics match {
            case Some(block) => vm.run(block)
            case None =>
          }
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

object TestParser extends FiniteAutoParser with App {
  println()
  println(" __________________________ ")
  println(" Choose a file with FSM ")
  println(" __________________________ ")
  var fileopen  = new JFileChooser();
  fileopen.setFileFilter(new FileNameExtensionFilter("*.fsm", "fsm"));
  //////////////////////////////////////
  //var ret = fileopen.showDialog(null, "Open File");
  var ret = JFileChooser.APPROVE_OPTION
  fileopen.setSelectedFile(new File("C:\\Users\\Dijkstra\\Documents\\test.fsm"))
  //////////////////////////////////////
  if (ret == JFileChooser.APPROVE_OPTION) {
    var in = new BufferedReader(new FileReader(fileopen.getSelectedFile()));
    parseAll(fsm, in) match {
      case Success(res,_) => {
        println(" FSM loaded. Awaiting input")
        var driver = new FSMDriver(res)
        println("Current state " + driver.state)
        var scanner = new Scanner(System.in)
        var nextLetter : String = scanner.nextLine()
        try {

          while (nextLetter.length == 1) {
            driver.next(nextLetter)
            println("Current state " + driver.state)
            nextLetter = scanner.nextLine()
            if (nextLetter.isEmpty) nextLetter = "f"
          }
        } catch {
          case msg => {
            println(msg)
          }
        }
        driver.finalActions
        //println(driver.unloadFSM)
      }
      case x => {
        println(" File parser faulure: ")
        println(x)
      }
    }
  }
}