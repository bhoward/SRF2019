name := "blogickFX"

organization := "edu.depauw"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.0"

libraryDependencies ++= Seq(
  "org.scalafx"   %% "scalafx"   % "12.0.2-R18",
  "com.lihaoyi"   %% "fastparse" % "2.1.3",
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test" //http://www.scalatest.org/download
)

// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

// Add JavaFX dependencies
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= javaFXModules.map( m=>
  "org.openjfx" % s"javafx-$m" % "12.0.2" classifier osName
)

// Fork a new JVM for 'run' and 'test:run' to avoid JavaFX double initialization problems
fork := true

// set the main class for the main 'run' task
// change Compile to Test to set it for 'test:run'
mainClass in (Compile, run) := Some("edu.depauw.blogick.BlogickFX")

shellPrompt := { _ => System.getProperty("user.name") + "> " }
