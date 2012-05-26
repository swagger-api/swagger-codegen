import com.wordnik.swagger.codegen.BasicGenerator

import com.wordnik.swagger.core._

object WordnikApiJavaCodegen extends JavaCodegen {
  def main(args: Array[String]) = generateClient(args)

  override def packageName = "com.wordnik.client"

  def destinationRoot = "wordnik-api/java"

  // where to write generated code
  override def destinationDir =  destinationRoot + "/src/main/java"

  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // supporting classes
  override def supportingFiles =
    List(
      ("apiInvoker.mustache", destinationRoot + "/src/main/java/com/wordnik/client", "ApiInvoker.java"),
      ("partOfSpeech.mustache", destinationRoot + "/src/main/java/com/wordnik/client/models", "PartOfSpeech.java"),
      ("facetValue.mustache", destinationRoot + "/src/main/java/com/wordnik/client/models", "FacetValue.java"),
      ("apiException.mustache", destinationRoot + "/src/main/java/com/wordnik/client", "ApiException.java"),
      ("pom.mustache", destinationRoot, "pom.xml")
    )
}
