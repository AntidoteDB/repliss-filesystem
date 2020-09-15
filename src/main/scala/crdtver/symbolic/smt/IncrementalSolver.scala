package crdtver.symbolic.smt

import java.time.{Duration, LocalDateTime, Period}
import java.time.temporal.TemporalAmount

import codes.reactive.scalatime._
import crdtver.symbolic.smt.Smt.NamedConstraint
import crdtver.symbolic.smt.Solver._
import crdtver.utils.{ConcurrencyUtils, TimeTaker}
import crdtver.utils.DurationUtils.DurationExt
import crdtver.utils.ListExtensions.ListUtils

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

/**
 * Runs a solver incrementally with increasing number of constraints.
 * Starting with most important constraints based on priority.
 */
class IncrementalSolver(
  subSolver: Solver
) extends Solver {

  override def toString: String = s"I$subSolver"

  override def check(constraints: List[Smt.NamedConstraint], options: List[SmtOption], name: String): CheckRes = {
    val timeOut: Duration = options.extract { case SmtTimeout(t) => t }.getOrElse(2.minutes)
    val maxEndTime: LocalDateTime = LocalDateTime.now().plus(timeOut)
    val options2 = options.filter(!_.isInstanceOf[SmtTimeout])

    @tailrec
    def explore(activeConstraints: List[NamedConstraint], extraConstraints: List[NamedConstraint], lastModel: Option[Model]): CheckRes = {
      val timeoutDur = Duration.between(LocalDateTime.now(), maxEndTime)
      if (timeoutDur <= 0.seconds) {
        return Unknown()
      }
      val options3 = SmtTimeout(timeoutDur) :: options2

      subSolver.check(activeConstraints, options3, name) match {
        case s: Satisfiable =>
          if (extraConstraints.isEmpty) {
            s
          } else {
            val model = s.getModel
            // simple approach: just add all constraints with the next higher priority:
            val (newActive, newExtra) = extraConstraints.partition(_.priority <= extraConstraints.head.priority)
            explore(activeConstraints ++ newActive, newExtra, Some(model))

            // more complex approach (not working yet)

            // find the first constraint that is not satisfied by the model
//            val failedConstraint: Option[NamedConstraint] = extraConstraints.find { c =>
////              println(s"Trying ${c.description}")
//              model.evalQ(c.constraint).contains(false)
//            }

//            failedConstraint match {
//              case Some(constr) =>
//                println(s"Adding constraint ${constr.description}\n${constr.constraint}")
//                explore(constr :: activeConstraints, extraConstraints - constr, Some(model))
//
//              case None =>
//                println(s"All constraints satisfied")
//                // if all constraints are satisfied in the model, then
//                // s is already a complete model for all constraints
//                s
//            }
          }
        case Unknown() =>
          lastModel match {
            case Some(model) =>
              // use last model, although it might be incomplete
              new Satisfiable {
                override def isIncomplete: Boolean = true
                override def getModel: Model = model
              }
            case None =>
              Unknown()
          }
        case u =>
          u
      }


    }


    val (activeConstraints, extraConstraints) = constraints.partition(_.priority == 0)
    val extraConstraintsSorted = extraConstraints.sortBy(_.priority)
    explore(activeConstraints, extraConstraintsSorted, None)


  }

  override def exportConstraints(assertions: List[Smt.NamedConstraint], options: List[SmtOption]): String = subSolver.exportConstraints(assertions, options)
}
