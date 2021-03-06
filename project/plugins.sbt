logLevel := Level.Warn

//addSbtPlugin("io.spray" % "sbt-revolver" % "0.8.0")
//
//addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.14")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.4.0")
addSbtPlugin("ch.epfl.scala" % "sbt-web-scalajs-bundler" % "0.20.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")
resolvers += Resolver.bintrayRepo("oyvindberg", "converter")
addSbtPlugin("org.scalablytyped.converter" % "sbt-converter" % "1.0.0-beta27")