package crdtver

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util

import crdtver.parser.{LangLexer, LangParser}
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
import org.antlr.v4.runtime._


object Test {

  def main(args: Array[String]): Unit = {


    val inputFileStr: String = if (args.length == 0) {
      "examples/userbase.scala"
    } else {
      args(0)
    }
    val inputFile = new File(inputFileStr)
    if (!inputFile.exists()) {
      println(s"Input file $inputFileStr not found.")
      return
    }
    println(s"Reading input $inputFileStr")

    val input = io.Source.fromFile(inputFile).mkString

    println(input)


    val inStream = new ANTLRInputStream(input)
    val lex = new LangLexer(inStream)
    val tokenStream = new CommonTokenStream(lex)
    val parser = new LangParser(tokenStream)

    var errorCount = 0
    parser.addErrorListener(new ANTLRErrorListener {

      override def reportContextSensitivity(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, prediction: Int, configs: ATNConfigSet): Unit = {
      }

      override def reportAmbiguity(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, exact: Boolean, ambigAlts: util.BitSet, configs: ATNConfigSet): Unit = {
      }

      override def reportAttemptingFullContext(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, conflictingAlts: util.BitSet, configs: ATNConfigSet): Unit = {
      }

      override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: scala.Any, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
        errorCount += 1
        println(s"Error $errorCount line $line:$charPositionInLine $msg")
      }

    })


    val prog = parser.program()

    println(s"There were $errorCount parser errors.")
    if (errorCount > 0) {
      return
    }



    val s = prog.toStringTree(parser)
    println(s"parsed = $s")

    val inputProg = InputAst.transformProgram(prog)

//    println(s"input prog = $inputProg")

    val typer = new Typer()
    val typedInputProg = typer.checkProgram(inputProg)

//    println(s"typed input prog = $inputProg")


    val translator = new BoogieTranslation2(parser)
    val boogieProg = translator.transformProgram(typedInputProg)

//    println(s"BOOGIE: $boogieProg")

    val sb = new StringBuilder
    new BoogiePrinter().printProgram(boogieProg, sb)
//    println(s"OUT = $sb")

    new File("model").mkdirs()

    val boogieOutputFile = Paths.get("model/test.bpl")
    Files.write(boogieOutputFile, sb.toString().getBytes(StandardCharsets.UTF_8))

    println("Starting boogie")

    import sys.process._
    //val boogieResult: String = "boogie test.bpl /printModel:2 /printModelToFile:model.txt".!!
    val boogieResult: String = "boogie model/test.bpl /timeLimit:10 /errorLimit:1 -mv:model/model.txt".!!

    println("result: ")
    println(boogieResult)

    // read and present the model
    val modelInterpreter = new ModelInterpreter
    modelInterpreter.load("model/model.txt")

  }


}