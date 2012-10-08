package ru.dijkstra
import ru.dijkstra.ast._
import scala.util.parsing.combinator.RegexParsers

/**
 *  Parser
 */


// СЛЕДИТЕ ЗА РУКАМИ
class FiniteAutoParser extends RegexParsers {
  override type Elem = Char
  // Inner language
  def identifier  = """[_\p{L}][_\p{L}\p{Nd}]*""".r
  def integer     = """-{0,1}(0|[1-9]\d*)""".r    ^^ { _.toInt }
  def string      = "'.+'".r
  def invert      = "inv" ~> identifier           ^^ { InvertDigits(_) }
  def call        = "call" ~> identifier          ^^ { Call(_) }
  def failure     = "failure"                     ^^ { _ => Failure_t }
  def newvar      = "var" ~> identifier           ^^ { New_Var(_) }
  def print       = "print" ~> string             ^^ { Print_Const(_) }
  def assign      = (identifier <~ "=") ~ integer ^^ { case a~b => Assign(a, b) }
  def output      = "out" ~> identifier           ^^ { Print_Var(_) }
  def proc        = "proc" ~> identifier ~ block  ^^ { case a~b => Proc(a,b) }
  def op_sign     = ("*"|"+"|"-") ^^
    { case "*" => Mul case "-" => Sub case "+" => Add }
  def ar_op = identifier ~ (op_sign <~ "=") ~ (integer | identifier) ^^
    {
      case a~op_sign~(b : Int)    => Ar_Op_Const(a, b, op_sign)
      case a~op_sign~(b : String) => Ar_Op_Var(a, b, op_sign)
    }
  def ops : Parser[List[Statement]] =
    ((newvar | ar_op | print | output | failure | call | proc | assign | invert) <~ ";")*
  def block = "{" ~> ops <~ "}" ^^ { l => Block(l) }
  // FSM
  def tablenode   = ("'" ~> ".".r <~ "'") ~ identifier ~ opt(block) ^^ { case a~b~c => TableNode(a, b, c) }
  def state       = opt("accept"|"allow"|"decline") ~ (identifier <~ ":") ~ (tablenode*) ^^
    {
      case None~a~b => State(true, a, b)
      case Some(a)~b~c => a match {
         case "allow"   => State(true, b, c)
         case "accept"  => State(true, b, c)
         case "decline" => State(false, b, c)
      }
    }
  def fsm         = opt(block) ~ (state*) ~ opt(block) ^^ { case a~b~c => FSM(a, b, c)  }
}
