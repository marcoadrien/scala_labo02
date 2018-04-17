// Laboratoire 02 - Calculatrice
// Modification: Adrien Marco, Julien Brêchet, Loan Lassalle
package calculator

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine

object Main {

  var memory: Map[String, Double] = Map()

  def main(args: Array[String]): Unit = {
    println("Welcome to the Scala calculator !")
    console()
  }

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
                                  "-    =>    substraction (example: 2 - 1)\n" +
                                  "*    =>    multiplication (example: 2 * 1)\n" +
                                  "/    =>    division (example: 2 / 1)\n" +
                                  "%    =>    modulo (example 2 % 1)\n" +
                                  "^    =>    pow (example: 2 ^ 1)\n" +
                                  "!    =>    factorial (example: 2 ! 1)\n" +
                                  "()   =>    parenthesis (example: (2 + 1) / 4 )\n" +
                                  "=    =>    variable afectation (example: my_variable = 666   =>   my_variable can be reused like this for example: my_variable * 2)")
    println("\nSUPPORTED FUNCTIONS: \nsqrt      =>    square root (example: sqrt(4)   =>   there is a default precision parameter of 0.0001)\n" +
                                    "gcd       =>    greatest common divisor (example: gcd(2,3))\n" +
                                    "modInv    =>    modular inverse of u mod v, modInv(u,v) (example: modInv(2,3))")
    println("\nCAUTION: some expressions can be ignored if there is a bad input like for example :" +
            "\n3!2   =>   2 is not considered")

    println("\nPLEASE ENTER YOUR CALCULATION AND PRESS ENTER TO SHOW THE RESULT...")
  }
  //--------------------------------------------------------------------------------------------------------------------

}
