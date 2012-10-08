package ru.dijkstra.fsa
import javax.swing.JFileChooser
import javax.swing.filechooser.FileNameExtensionFilter
import java.io.{File, FileReader, BufferedReader}
import java.util.Scanner
import scalax.io._

import ru.dijkstra.{FSMDriver, FiniteAutoParser}


object TestParser extends FiniteAutoParser with App {
  println()
  println()
  println("[run] Choose a file with FSM ")
  var fileopen  = new JFileChooser();
  fileopen.setFileFilter(new FileNameExtensionFilter("*.fsm", "fsm"));
  //////////////////////////////////////
  var ret = fileopen.showDialog(null, "Open File");
  //var ret = JFileChooser.APPROVE_OPTION
  //fileopen.setSelectedFile(new File("C:\\Users\\Dijkstra\\Documents\\test.fsm"))
  //////////////////////////////////////
  if (ret == JFileChooser.APPROVE_OPTION) {
    var in = new BufferedReader(new FileReader(fileopen.getSelectedFile()));
    parseAll(fsm, in) match {
      case Success(res,_) => {
        var driver = new FSMDriver(res)
        println("[run] FSM loaded. Awaiting input")
        println("[run] FSM input alphabet:  " + driver.inputAlphabet)
        println("[run] Current state " + driver.state)
        var scanner = new Scanner(System.in)
        var nextLetter : String = scanner.nextLine()
        try {
          while (nextLetter.length == 1) {
            driver.next(nextLetter)
            println("[run] Current state: " + driver.state + " String accepted: " + driver.allowed)
            nextLetter = scanner.nextLine()
          }
        } catch {
          case msg => {
            println(msg)
          }
        }
        driver.finalActions
        driver.normalForm
        val unloadto = fileopen.getSelectedFile().getPath() + ".optimized.fsm"
        val output:Output = Resource.fromFile(unloadto)
        output.write(driver.unloadFSM.toString())(Codec.UTF8)
        println("[run] optimized FSM unloaded into:\n" + unloadto)
      }
      case x => {
        println("[error] File parser faulure: ")
        println(x)
      }
    }
  }
}