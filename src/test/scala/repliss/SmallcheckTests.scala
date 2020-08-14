package repliss

import crdtver.Repliss.{Quickcheck, ReplissResult, SmallCheck}
import crdtver.utils.Helper
import crdtver.{Repliss, RunArgs}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

/**
  * Tests for the random test generator
  */
class SmallcheckTests extends AnyFunSuite with Matchers {

  //  def checkResource(name: String): Result[ReplissResult] = {
  //    val input = Helper.getResource(name)
  //    Repliss.checkInput(input, name, runArgs = RunArgs())
  //  }

  private def checkString(name: String, input: String): ReplissResult = {
    val res = Repliss.checkInput(input, name, runArgs = RunArgs(), checks = List(SmallCheck()))
    res match {
      case Repliss.NormalResult(rr) =>
        Repliss.printTestingResultSmallCheck(rr, name, new Object())
        rr
      case Repliss.ErrorResult(errors) =>
        throw new RuntimeException(errors.map(_.toString).mkString("\n"))
    }
  }

  private def checkResource(name: String): ReplissResult = {
    val input = Helper.getResource(name)
    checkString(name, input)
  }


  test("no counterexample for userbase", Slow) {

    val res = checkResource("/examples/verified/userbase.rpls")

    assert(!res.hasSmallCheckCounterexample)
  }

  test("userbase_fail1 counterexample", Slow) {
    val res = checkResource("/examples/failsToVerify/userbase_fail1.rpls")

    assert(res.hasSmallCheckCounterexample)
  }

}
