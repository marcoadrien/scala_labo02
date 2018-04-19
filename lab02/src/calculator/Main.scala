/*
Laboratoire 02 - Calculatrice
Modification: Adrien Marco, Julien Brêchet, Loan Lassalle
This is the main class of the app. It initialize the memory and open the console to read the user input.
The calculator calls the parser to define the token and compute them. The parser will use the definitions of tokens in
Tokens.scala, the calculation tree expression in Trees.sacal and reading functions in Lexer.scala to do the job.
Trees.scala will define the calculation functions
known in the calculator and represents it as nodes in a tree where the values will be recursively evaluated.
*/
package calculator

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine

object Main {

  //represents the memory fo the calculator (mapping between variable names and values)
  var memory: Map[String, Double] = Map()

  def main(args: Array[String]): Unit = {
    println("Welcome to the Scala calculator !")
    console()
  }

  //reads the console input (3 types of possibilities quit/usage/calculation)
  @tailrec
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
    println("SUPPORTED OPERATORS: \n+    =>    addition (example: 2 + 1)\n" +
                                  "-    =>    subtraction (example: 2 - 1)\n" +
                                  "*    =>    multiplication (example: 2 * 1)\n" +
                                  "/    =>    division (example: 2 / 1)\n" +
                                  "%    =>    modulo (example 2 % 1)\n" +
                                  "^    =>    pow (example: 2 ^ 1)\n" +
                                  "!    =>    factorial (example: 2 ! 1)\n" +
                                  "()   =>    parenthesis (example: (2 + 1) / 4)\n" +
                                  "=    =>    variable affectation (example: my_variable = 666   =>   my_variable can be reused like this for example: my_variable * 2)")
    println("\nSUPPORTED FUNCTIONS: \nsqrt      =>    square root (example: sqrt(4)   =>   there is a default precision parameter of 0.0001)\n" +
                                    "gcd       =>    greatest common divisor (example: gcd(2,3))\n" +
                                    "modInv    =>    modular inverse of u mod v, modInv(u,v) (example: modInv(2,3))")
    println("\nPLEASE ENTER YOUR CALCULATION AND PRESS ENTER TO SHOW THE RESULT...")
  }
  //--------------------------------------------------------------------------------------------------------------------
}
