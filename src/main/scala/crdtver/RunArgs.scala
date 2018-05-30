package crdtver

import java.io.File

case class RunArgs(
  server: Boolean = false,
  quickcheck: Boolean = false,
  verify: Boolean = true,
  sessionIds: Boolean = true,
  host: String = "localhost",
  port: Int = 8080,
  file: Option[String] = None
) {

}

object RunArgs {

  def parse(args: List[String]): Option[RunArgs] = parser.parse(args, RunArgs())

  def printHelp(): Unit = parser.showUsage()

  private val parser = new scopt.OptionParser[RunArgs]("repliss") {
    head("Repliss")


    opt[Unit]("server")
      .action((v, args) => args.copy(server = true))
      .text("Starts a server")

    opt[String]('h', "host")
      .action((v, args) => args.copy(host = v))
      .text("Specifies on which host the server should run")

    opt[Int]('p', "port")
      .action((v, args) => args.copy(port = v))
      .text("Specifies on which port the server should run")


    opt[Unit]("quickcheck")
      .action((v, args) => args.copy(quickcheck = true))
      .text("Runs random tests on the input program")

    opt[Unit]("session-ids")
          .action((v, args) => args.copy(sessionIds = true))
          .text("Identifier-types are used with session guarantees, i.e. an invocation can only use an identifier-type, if all corresponding database operation has been applied on the database. Currently this is only supported for testing but not for verification.")

    opt[Unit]("noverify")
      .action((v, args) => args.copy(verify = false))
      .text("Do not run verification tasks.")

    arg[String]("<file>")
      .optional()
      .action( (v, args) => args.copy(file = Some(v)) )
      .text("File to parse")

  }
}
