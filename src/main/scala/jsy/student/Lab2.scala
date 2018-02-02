package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * Michael Gohde
   * 
   * Partner: Matt Waymouth
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case S(s) => s.toDouble
      case _ => ???
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case _ => ???
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case N(n) => n.toString()
      case B(b) => b.toString()
      case _ => ""
    }
  }

  def doMathBin(func: (Double, Double) => Double, e1: Expr, e2: Expr) = {
    e1 match {
      case N(n) => e2 match {
        case N(m) => N(func(n, m))
        case _ => Undefined
      }

      case _ => Undefined
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(n) => e
      case S(s) => e
      case B(b) => e
      case Undefined => Undefined

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
      case Unary(uop, e1) => uop match {
        case Not => eval(env, e1) match
          {
          case B(b) => B(!b)
          case _ => Undefined
          }
        case Neg => eval(env, e1) match
          {
          case N(n) => N(-n)
          case _ => Undefined
          }
      }

      case Binary(bop, e1, e2) => bop match
      {
        case Plus => {
          val ee1=eval(e1)
          val ee2=eval(e2)

          doMathBin((a: Double, b: Double) => a+b, ee1, ee2)
        }

        case Minus => {
          val ee1=eval(e1)
          val ee2=eval(e2)

          doMathBin((a: Double, b:Double) => a-b, ee1, ee2)
        }

        case Times => {
          val ee1=eval(e1)
          val ee2=eval(e2)

          doMathBin((a: Double, b: Double) => a*b, ee1, ee2)
        }

        case Div => {
          val ee1=eval(e1)
          val ee2=eval(e2)

          doMathBin((a: Double, b:Double) => a/b, ee1, ee2)
        }
        case Eq => ???
        case Ne => ???
        case Lt => ???
        case Le => ???
        case Gt => ???
        case Ge => ???
        case And => ???
        case Or => ???
        case Seq => ???
      }

      case If(e1, e2, e3) => eval(e1) match
        {
        case B(b) => if(b) eval(env, e2) else eval(env, e3)
        case _ => Undefined
      }

      case _ => ???
    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}
