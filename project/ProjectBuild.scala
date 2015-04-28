import sbt._
import sbt.Keys._

object ProjectBuild extends Build {
  import Settings._

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = parentSettings,
    aggregate = Seq(sarumanDsl, sarumanCli, sarumanApp)
  )

  lazy val sarumanDsl = Project(
    id = "saruman-dsl",
    base = file("./saruman-dsl"),
    settings = defaultSettings ++ Seq(libraryDependencies ++= Dependencies.sarumanDsl)
  )

  lazy val sarumanCli = Project(
    id = "saruman-cli",
    base = file("./saruman-cli"),
    settings = defaultSettings ++ Seq(libraryDependencies ++= Dependencies.sarumanCli)
  ).dependsOn(sarumanDsl)

  lazy val sarumanApp = Project(
    id = "saruman-app",
    base = file("./saruman-app"),
    settings = defaultSettings ++ Seq(libraryDependencies ++= Dependencies.sarumanApp)
  ).dependsOn(sarumanDsl)
}

object Dependencies {
  import Versions._

  object Compile {
    val config        = "com.typesafe"             % "config"               % TypesafeConfigVer
    val ssh           = "com.decodified"          %% "scala-ssh"            % ScalaSshVer
    val logback       = "ch.qos.logback"           % "logback-classic"      % LogbackVer
    val bouncy        = "org.bouncycastle"         % "bcprov-jdk16"         % "1.46"
    val jcraft        = "com.jcraft"               % "jzlib"                % "1.1.3"
  }

  object Test {
    val scalatest     = "org.scalatest"           %% "scalatest"            % ScalaTestVer      % "test"
    val scalacheck    = "org.scalacheck"          %% "scalacheck"           % ScalaCheckVer     % "test"
    val junit         = "junit"                    % "junit"                % JunitVer          % "test"

    val abideExtra    = "com.typesafe"             % "abide-extra_2.11"     % AbideExtraVer     % "abide,test"
  }

  import Compile._

  val test = Seq(Test.scalatest, Test.scalacheck, Test.junit)

  /** Module deps */

  val sarumanDsl = Seq(config, ssh, logback, bouncy, jcraft) ++ test
  val sarumanCli = Seq(config) ++ test
  val sarumanApp = Seq(config) ++ test
}
