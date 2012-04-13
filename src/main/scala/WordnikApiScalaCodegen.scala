import com.wordnik.swagger.codegen.BasicGenerator

import com.wordnik.swagger.core._

object WordnikApiScalaCodegen extends ScalaCodegen {
  def main(args: Array[String]) = generateClient(args)

  override def packageName = "com.wordnik.client"
    
  // where to write generated code
  override def destinationDir = "generated-code/wordnik-api-scala/src/main/scala"

  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // supporting classes
  override def supportingFiles = List(
    ("apiInvoker.mustache", "generated-code/wordnik-api-scala/src/main/scala/com/wordnik/client", "ApiInvoker.scala"),
    ("pom.mustache", "generated-code/wordnik-api-scala", "pom.xml")
  )
}