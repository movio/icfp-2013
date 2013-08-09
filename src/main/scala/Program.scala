import Parser.parse
import Interpreter.eval

object Program extends App {

  // run some program with the given input

  eval(
    "(lambda (x) (fold x 0 (lambda (y z) (or (shl1 (shl1 (shl1 (shl1 (shl1 (shl1 (shl1 (shl1 z)))))))) y))))",
    0xFF00000000000000L,
    0xFF00FF00FF00FF00L
  )

  eval(
    "(lambda (x) (plus x 1))",
    1,
    2,
    3,
    4,
    5
  )

  // get (and print) the AST from the parser

  println(parse("(lambda (x) (fold x 0 (lambda (y z) (if0 y y y))))"))
}
