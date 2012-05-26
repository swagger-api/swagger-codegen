import com.wordnik.swagger.codegen.BasicGenerator

import com.wordnik.swagger.core._

object WordnikApiScalaCodegen extends ScalaCodegen {
  def main(args: Array[String]) = generateClient(args)

  override def packageName = "com.wordnik.client"

  def destinationRoot = "wordnik-api/scala"

  // where to write generated code
  override def destinationDir = destinationRoot + "/src/main/scala"

  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // supporting classes
  override def supportingFiles = List(
    ("apiInvoker.mustache", destinationRoot + "/src/main/scala/com/wordnik/client", "ApiInvoker.scala"),
    ("pom.mustache", destinationRoot, "pom.xml"),
    ("facetValue.mustache", destinationRoot + "/src/main/scala/com/wordnik/client/model", "FacetValue.scala"),
    ("partOfSpeech.mustache", destinationRoot + "/src/main/scala/com/wordnik/client/model", "PartOfSpeech.scala")
  )
}