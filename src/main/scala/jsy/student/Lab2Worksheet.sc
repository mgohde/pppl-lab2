/*
 * CSCI 3155: Lab 2 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab2.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab2.Parser.parse

// Imports the ast nodes
import jsy.lab2.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab2._

// Call the JavaScripty parser (from the provided library) on a string
val negFourAST = parse("-4")

// Evaluate that JavaScripty expression.
eval(negFourAST)

// For convenience, we also have an eval function that takes a string,
// which calls the parser and then delegates to your eval function.
//eval("undefined + 1")

val testAst=parse("const x=1; const y=2; const z=3; (x+y+z) ? 4 : 0")
val output=eval(testAst)

val ifAst=parse("1 ? 5+1 : 0")
val iftest=eval(parse("1 ? 5+1 : 0"))
