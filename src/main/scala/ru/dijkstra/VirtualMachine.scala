package ru.dijkstra
import ru.dijkstra.ast._
import collection.mutable

/**
 * Inner language interpreter
 */

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
