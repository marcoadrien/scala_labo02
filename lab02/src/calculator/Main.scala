// Laboratoire 02 - Calculatrice
// Modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator

import scala.io.Source
import scala.io.StdIn.readLine

object Main {

  var memory: Map[String, Double] = Map()

  def main(args: Array[String]): Unit = {
    println("Welcome to the Scala calculator !")
    console()
  }

  def console(): Unit = {
    readLine match {
      case "quit" => println("Bye !")
      case "usage" =>
        usage()
        console()
      case s =>
        new Calculator(Source.fromString(s)).execute()
        console()
    }
  }

  //----------------------------------------------------------------------------------------------------------------------
  /** show help commands */
  def usage(): Unit = {
    println("supported operators: \n+\n-\n*\n/\n%\n^\n!\n()\n=")
    println("supported functions: \nsqrt\ngcd\nmodInv")
  }

  //--------------------------------------------------------------------------------------------------------------------
}