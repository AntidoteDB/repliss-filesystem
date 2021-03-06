package crdtver.testing

import java.io.{ByteArrayInputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.Base64

import crdtver.language.TypedAst.{IdType, InProgram}
import crdtver.testing.Interpreter.{AnyValue, CallId, DataTypeValue, InvocationId, InvocationInfo, State}

import scala.xml.Elem

object Visualization {

  def reducedStateGraph(prog: InProgram, state: State): String = {
    val sb = new StringBuilder()

    def p(s: String): Unit = {
      sb.append(s)
      sb.append("\n")
    }

    printStateGraph(state, p)
    val dot = sb.toString()
    val dotIs = new ByteArrayInputStream(dot.getBytes(StandardCharsets.UTF_8))

    import sys.process._
    val reducedDot: String = ("tred" #< dotIs).!!

    sb.clear()
    sb.append(reducedDot.substring(0, reducedDot.length - 2))
    addInvocationDependencies(prog, state, p)
    sb.append("}")
    sb.toString()
  }

  case class RenderResult(
    dot: String,
    svg: String,
    pdf: Array[Byte]
  ) {
    def toXml: Elem = {
      // TODO maybe add correction:
      //      val svg2 = svg.replace("14.00", "10.5pt")
      <render>
        <dot>
          {dot}
        </dot>
        <svg>
          {svg}
        </svg>
        <pdf>
          {Base64.getEncoder.encodeToString(pdf)}
        </pdf>
      </render>
    }

  }

  def renderStateGraph(prog: InProgram, state: State): RenderResult = {
    val reducedDot = reducedStateGraph(prog, state)


    def dotIs2 = new ByteArrayInputStream(reducedDot.getBytes(StandardCharsets.UTF_8))

    import sys.process._
    val outputSvg: String = ("dot -Tsvg" #< dotIs2).!!


    var outputPdf: Option[Array[Byte]] = None
    val io = new ProcessIO(
      writeInput = (is: OutputStream) => {},
      processOutput = (is: InputStream) => {
        outputPdf = Some(is.readAllBytes())
      },
      processError = (is: InputStream) => {},
      daemonizeThreads = false
    )
    val proc = ("dot -Tpdf" #< dotIs2).run(io)
    proc.exitValue()
    //    debugLog(s"SVG output = $output")
    return RenderResult(reducedDot, outputSvg, outputPdf
      .getOrElse(throw new RuntimeException("pdf output not available yet")))
  }


  def printStateGraphToFile(prog: InProgram, state: State, filename: String): Unit = {
    val reducedDot = reducedStateGraph(prog, state)

    Files.write(Paths.get(s"model/graph_$filename.dot"), reducedDot.getBytes(StandardCharsets.UTF_8))
    import sys.process._
    (s"tred model/graph_$filename.dot" #| s"dot -Tsvg -o model/graph_$filename.svg").!

    (s"tred model/graph_$filename.dot" #| s"dot -Tpdf -o model/graph_$filename.pdf").!
  }

  def printStateGraph(state: State, p: (String) => Unit): Unit = {

    p(s"digraph G {")
    p("   graph [splines=true overlap=false]")
    for (i <- state.invocations.values) {
      var containsCall = false
      p(
        s"""subgraph cluster_${i.id} {
           |   style="rounded,filled,dashed";
           |   color="#000000";
           |   fillcolor="#eeeeff";
           |   node [style="filled,dashed", color=white]
           |   label = "${i.id}
           |      ${i.operation}
           |      result: ${i.result.getOrElse("-")}"
         """.stripMargin)
      val txns = state.transactions.values.filter(_.origin == i.id)

      state.localStates.get(i.id).map(_.currentTransaction) match {
        case Some(tx) =>
          p(s"/* current tx = ${tx} */")
        case None =>
          p(s"/* no current tx */")
      }

      for (tx <- txns) {
        p(
          s"""subgraph cluster_${tx.id} {
             |   style="rounded,filled,dotted";
             |   color="#000000";
             |   fillcolor="#eeffee";
             |   node [style="filled,dotted", color=white, shape=box, style="rounded,filled"]
             |   label = "${tx.id}"
                 """.stripMargin)
        val calls = state.calls.values.filter(_.callTransaction == tx.id)
        p(s"/* ${tx.id} calls = ${calls} */")
        p(s"/* ${tx.id} currentCalls = ${tx.currentCalls} */")
        for (c <- calls) {
          val opStr =
            if (c.operation.operationName.startsWith("queryop_")) {
              val op = DataTypeValue(
                operationName = c.operation.operationName.drop("queryop_".length),
                args = c.operation.args.take(c.operation.args.size - 1)
              )
              val res = c.operation.args.last
              s"$op\nresult: $res"
            } else {
              c.operation.toString
            }

          p(s"""${c.id}[label="${c.id}\n$opStr", style="rounded,filled,solid", color="#666666", fillcolor="#ffffff"];""")
          containsCall = true
        }
        p(s"""}""")
      }
      if (!containsCall) {
        p(s"empty_${i.id}")
      }

      p("}")
    }

    // add dependencies
    for (c <- state.calls.values) {
      for (dep <- c.callClock.snapshot) {
        p(s"${dep} -> ${c.id};")
      }
      p("")
    }


    p("}")


  }


  private def addInvocationDependencies(prog: InProgram, state: State, p: (String) => Unit): Unit = {
    // add invisible-id dependencies:
    val callsInInvocation: Map[InvocationId, Seq[CallId]] = state.calls.values
      .groupBy(ci => ci.origin)
      .view
      .mapValues(calls => calls.map(_.id).toSeq.sorted)
      .toMap


    var idOrigin = Map[(IdType, AnyValue), InvocationId]()
    val sortedInvocations: Seq[InvocationInfo] = state.invocations.values.toList.sortBy(_.id)
    for (invoc <- sortedInvocations) {
      invoc.result match {
        case Some(DataTypeValue(_, List(res))) =>
          val proc = prog.findProcedure(invoc.operation.operationName)
          val returnedIds = Interpreter.extractIds(res, proc.returnType, prog)
          for ((idt, idvals) <- returnedIds; idval <- idvals) {
            if (!idOrigin.contains((idt, idval))) {
              idOrigin += ((idt, idval) -> invoc.id)
            }
          }
        case None =>
      }
    }


    for (invoc <- sortedInvocations) {
      prog.tryFindProcedure(invoc.operation.operationName) match {
        case None =>
        case Some(proc) =>

          val argIds = Interpreter.extractIdsList(invoc.operation.args, proc.params.map(_.typ), prog)
          for ((idt, idvals) <- argIds; idval <- idvals) {

            idOrigin.get((idt, idval)) match {
              case Some(id) =>
                for {
                  calls1 <- callsInInvocation.get(id)
                  c1 <- calls1.lastOption
                  calls2 <- callsInInvocation.get(invoc.id)
                  c2 <- calls2.headOption
                } {
                  //              println("$c1 -> $c2;")
                  p(s"""  $c1 -> $c2 [style=invis];""")
                }
              case None =>
            }
          }
      }
    }
  }


}
