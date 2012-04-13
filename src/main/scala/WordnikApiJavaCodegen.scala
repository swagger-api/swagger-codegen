import com.wordnik.swagger.codegen.BasicGenerator

import com.wordnik.swagger.core._

object WordnikApiJavaCodegen extends JavaCodegen {
  def main(args: Array[String]) = generateClient(args)

  override def packageName = "com.wordnik.client"
    
  // where to write generated code
  override def destinationDir = "samples/wordnik-api-java"

  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // supporting classes
  override def supportingFiles =
    List(
      ("apiInvoker.mustache", "samples/wordnik-api-java/src/main/java/com/wordnik/client", "ApiInvoker.java"),
      ("apiException.mustache", "samples/wordnik-api-java/src/main/java/com/wordnik/client", "ApiException.java"),
      ("pom.mustache", "samples/wordnik-api-java", "pom.xml")
    )
}