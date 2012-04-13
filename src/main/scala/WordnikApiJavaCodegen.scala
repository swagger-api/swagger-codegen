import com.wordnik.swagger.codegen.BasicGenerator

import com.wordnik.swagger.core._

class WordnikApiJavaCodegen extends JavaCodegen {
  override def packageName = "com.wordnik.client"
    
  // where to write generated code
  override def destinationDir = "src/test/java"

  // package for models
  override def modelPackage = Some("com.wordnik.javaPetstore.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.javaPetstore.api")

  // supporting classes
  override def supportingFiles =
    List(
      ("apiInvoker.mustache", "src/test/java/com/wordnik/javaPetstore", "ApiInvoker.java"),
      ("apiException.mustache", "src/test/java/com/wordnik/javaPetstore", "ApiException.java"),
      ("pom.mustache", "src/test/java", "pom.xml")
    )
}