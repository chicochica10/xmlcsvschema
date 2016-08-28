name := "xmlcsvschema"

version := "1.0"

scalaVersion := "2.10.6"


resolvers ++= Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")


libraryDependencies ++= Seq(
  //add provided for spark dependencies when uploading
  "com.typesafe" % "config" % "1.3.0",
  "org.apache.spark" % "spark-core_2.10" % "1.6.0" % "provided",
  "org.apache.spark" % "spark-sql_2.10" % "1.6.0" % "provided",
  "org.apache.spark" % "spark-mllib_2.10" % "1.6.0" % "provided",
  "org.apache.spark" % "spark-streaming_2.10" % "1.6.0" % "provided",
  "org.apache.spark" % "spark-hive_2.10" % "1.6.0" % "provided",
  "com.databricks" % "spark-csv_2.10" % "1.4.0"

)

assemblyJarName in assembly := "smallFiles.jar"

mainClass in assembly := Some ("com.imshealthcare.LoadSmallFiles")

