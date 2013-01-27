name := "pc"

version := "1.0-SNAPSHOT"

organization := "net.entelijan"

scalaVersion := "2.10.0"



resolvers ++= Seq(
	"typesave" at "http://repo.typesafe.com/typesafe/releases")
	
libraryDependencies ++= Seq(
        "org.scalatest" % "scalatest_2.10" % "1.9.1")
