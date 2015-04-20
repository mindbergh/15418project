import java.io.{PrintStream, Reader}

import scala.util.parsing.combinator.RegexParsers

abstract class Operation
case object IncOp extends Operation
case object DecOp extends Operation
case object NextOp extends Operation
case object PrevOp extends Operation
case object OutOp extends Operation
case object InOp extends Operation
case class Loop(val operations: Seq[Operation]) extends Operation



object bfParser extends RegexParsers {
  def ignore = """[^<>+-\.,\[\]]""".r
  def inc = "+" ^^ { s => IncOp }
  def dec = "-" ^^ { s => DecOp }
  def next = ">" ^^ { s => NextOp }
  def prev = "<" ^^ { s => PrevOp }
  def out = "." ^^ { s => OutOp }
  def in = "," ^^ { s => InOp }
  def op = (ignore *) ~> (inc | next | prev | dec | out) <~ (ignore *)
  def loop = "[" ~> (op *) <~ "]" ^^ { l => Loop(l) }
  def prog = (op | loop) *
}



class BFInterpreter(val stdin: Reader = Console.in, val stdout: PrintStream = Console.out) {
  private final val memory = scala.collection.mutable.ArrayBuffer.fill[Byte](30000) { 0 }
  private final var pointer = 0

  def run(program: Seq[Operation]): Unit = {
    for (op <- program)
      op match {
        case IncOp => memory(pointer) = (memory(pointer) + 1).toByte
        case DecOp => memory(pointer) = (memory(pointer) - 1).toByte
        case NextOp => pointer += 1
        case PrevOp => if (pointer > 0) pointer -= 1 else pointer = 0
        case Loop(l) => do { run(l) } while (memory(pointer) != 0)
        case OutOp => stdout.print(memory(pointer).toChar)
        case InOp => memory(pointer) = stdin.read().toByte
      }
  }
  def run(program: String) {
    import bfParser._
    bfParser.parseAll(bfParser.prog, program) match {
      case Success(p, _) => run(p)
      case NoSuccess(msg, _) => throw new IllegalArgumentException("Could not parse program: " + msg)
    }
  }
}

object Main extends App {
  val p = """
+++++ +++++             initialize counter (cell #0) to 10
[                       use loop to set the next four cells to 70/100/30/10
    > +++++ ++              add  7 to cell #1
    > +++++ +++++           add 10 to cell #2 
    > +++                   add  3 to cell #3
    > +                     add  1 to cell #4
    <<<< -                  decrement counter (cell #0)
]                   
> ++ .                  print 'H'
> + .                   print 'e'
+++++ ++ .              print 'l'
.                       print 'l'
+++ .                   print 'o'
> ++ .                  print ' '
<< +++++ +++++ +++++ .  print 'W'
> .                     print 'o'
+++ .                   print 'r'
----- - .               print 'l'
----- --- .             print 'd'
> + .                   print '!'
> .                     print '\n
          """
  new BFInterpreter().run(p)

}