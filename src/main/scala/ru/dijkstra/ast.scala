package ru.dijkstra.ast

/**
 * AST
 */

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
case class State(allow : Boolean, name: String, tableRow: List[TableNode])
case class FSM(before: Option[Block], states:List[State], after:Option[Block])
