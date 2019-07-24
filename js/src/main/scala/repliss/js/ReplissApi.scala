package repliss.js


import fr.hmil.roshttp.HttpRequest
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom.raw.{DOMParser, Document, Element, HTMLCollection, XMLHttpRequest}
import repliss.js.Data.{CounterExample, QuickCheckCounterExample, QuickCheckResult, QuickCheckResultOk, ReplissError, ReplissResult, ResultState, TraceStep, VerificationError, VerificationResult}
import rx.Var

import scala.concurrent.Future
import scala.scalajs.js.Dynamic.literal
import scala.scalajs.js.JSON
import scala.util.{Failure, Success}
import scala.scalajs.js.timers._

object ReplissApi {
  private val endpoint = "//localhost:8080/api"

  private implicit class HTMLCollectionExt(c: HTMLCollection) {
    def foreach(f: Element => Unit): Unit = {
      for (i <- 0 until c.length)
        f(c(i))
    }

    def map[T](f: Element => T): List[T] = {
      (for (i <- 0 until c.length) yield f(c(i))).toList
    }

    def flatMap[T](f: Element => TraversableOnce[T]): List[T] = {
      (0 until c.length).flatMap(i => f(c(i))).toList
    }

    def headOption: Option[Element] =
      if (c.length > 0)
        Some(c(0))
      else None

    def toList: List[Element] =
      map(identity)

  }

  private implicit class ElementExt(e: Element) {


    def selectTag(name: String): List[Element] = {
      e.children.toList.filter(c => c.tagName == name)
    }

    def attr(name: String): String =
      try {
        val r = e.getAttribute(name)
        if (r == null)
          throw new RuntimeException(s"Attribute $name was null")
        r
      } catch {
        case e: Throwable =>
          throw new RuntimeException(s"Could not read attribute $name", e)
      }

    def txt: Option[String] =
      try {
        val r = e.innerText
        if (r == null) {
          Console.println(s"Could not get inner text")
          None
        } else Some(r)
      } catch {
        case e: Throwable =>
          Console.println(s"Error when getting inner text")
          e.printStackTrace()
          html
      }

    def html: Option[String] =
      try {
        val r = e.innerHTML
        if (r == null) {
          Console.println(s"Could not get inner html")
          None
        } else Some(r)
      } catch {
        case e: Throwable =>
          Console.println(s"Error when getting inner html")
          e.printStackTrace()
          None
      }

  }

  def parseResultState(str: String): Data.ResultState = str match {
    case "valid" => ResultState.Valid
    case "error" => ResultState.Error
    case "unknown" => ResultState.Unknown
  }

  def parseTranslation(t: Element): Data.Translation = {
    Data.Translation(
      t.attr("name"),
      t.getElementsByTagName("isabelle").headOption.flatMap(e => e.txt).getOrElse(""),
      t.getElementsByTagName("smt").headOption.flatMap(e => e.txt).getOrElse("")
    )
  }

  def parseTranslations(collection: List[Element]): List[Data.Translation] =
    for (t <- collection) yield
      parseTranslation(t)

  def parseError(p: Element): ReplissError = {
    ReplissError(
      Integer.parseInt(p.attr("line")),
      Integer.parseInt(p.attr("column")),
      Integer.parseInt(p.attr("endline")),
      Integer.parseInt(p.attr("endcolumn")),
      p.attr("message")
    )
  }

  def parseCounterExample(e: Element): Data.CounterExample =
    CounterExample(
      e.selectTag("modelText").head.txt.getOrElse(""),
      e.selectTag("counterExampleSvg").head.html.getOrElse("")
    )

  def parseStep(e: Element): TraceStep =
    TraceStep(
      Integer.parseInt(e.attr("line")),
      e.attr("description"),
      parseCounterExample(e.selectTag("counterExample").head)
    )

  def parseTrace(e: Element): List[Data.TraceStep] = {
    e.selectTag("step").map(parseStep)
  }

  def parseVerificationError(e: Element): Data.VerificationError = {


    VerificationError(
      e.attr("message"),
      parseTrace(e.selectTag("trace").head),
      parseTranslation(e.selectTag("translation").head)
    )
  }

  def parseVerificationErrors(elements: List[Element]): Option[Data.VerificationError] =
    elements.headOption.map(parseVerificationError)

  def parseReplissResult(xml: Document): ReplissResult = {
    val root = xml.documentElement

    val errors = for (errors <- root.selectTag("errors"); p <- errors.children) yield parseError(p)

    val procNames = for (ps <- root.selectTag("procedures"); p <- ps.children)
      yield {
        p.attr("name")
      }


    val vr = for (p <- root.selectTag("verificationResult")) yield {
      VerificationResult(
        p.attr("proc"),
        p.attr("time"),
        parseResultState(p.attr("resState")),
        parseTranslations(p.getElementsByTagName("translation").toList),
        parseVerificationErrors(p.selectTag("error"))
      )
    }

    val quickCheckResult: Option[QuickCheckResult] =
      if (root.selectTag("nocounterexample").nonEmpty) {
        Some(QuickCheckResultOk)
      } else root.selectTag("counterexample").headOption.map(ce => {
        QuickCheckCounterExample(
          Integer.parseInt(ce.attr("invline")),
          ce.attr("info"),
          ce.txt.getOrElse("visualization failed")
        )
      })




    ReplissResult(
      procNames,
      vr,
      errors,
      quickCheckResult
    )
  }

  def check(code: String): Var[ReplissResult] = {
    var xhr = new XMLHttpRequest()
    xhr.open("POST", s"$endpoint/check", async = true)
//    xhr.open("GET", s"/api/check6.xml", async = true)
    xhr.send(JSON.stringify(literal(code = code)))

    val result = Var(ReplissResult())

    var lastText = ""
    var timer: SetIntervalHandle = null
    timer = setInterval(1000) {
      if (xhr.readyState == XMLHttpRequest.DONE) {
        clearInterval(timer)
      }
      val incompleteText = xhr.responseText
      if (incompleteText.length > lastText.length && incompleteText.indexOf("<results") >= 0) {
        lastText = incompleteText

        val text =
          if (incompleteText.indexOf("</results>") < 0)
            incompleteText + "</results>"
          else
            incompleteText

        val parser = new DOMParser()

        val xml = parser.parseFromString(text, "text/xml")
        result.update(parseReplissResult(xml))
      }
    }
    result
  }


  def getExamples: Future[Either[Error, List[Data.Example]]] = {
    val request = HttpRequest(s"$endpoint/examples")
    request.send().map(r => {
      val res = Json.decode[List[Data.Example]](r.body)
      res
    })



  }


}