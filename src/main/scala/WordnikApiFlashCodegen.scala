object WordnikApiFlashCodegen extends FlashCodegen {
  def main(args: Array[String]) = generateClient(args)

  override def packageName = "com.wordnik.client"

  def destinationRoot = "wordnik-api/flash"

  // where to write generated code
  override def destinationDir = destinationRoot + "/src/main/flex"

  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // supporting classes
  override def supportingFiles = List(
    ("ApiInvoker.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/common", "ApiInvoker.as"),
    ("ApiUrlHelper.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/common", "ApiUrlHelper.as"),
    ("ApiUserCredentials.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/common", "ApiUserCredentials.as"),
    ("ListWrapper.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/common", "ListWrapper.as"),
    ("SwaggerApi.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/common", "SwaggerApi.as"),
    ("XMLWriter.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/common", "XMLWriter.as"),

    ("ApiError.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/exception", "ApiError.as"),
    ("ApiErrorCodes.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/exception", "ApiErrorCodes.as"),

    ("ApiClientEvent.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/event", "ApiClientEvent.as"),
    ("Response.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/event", "Response.as"),

    ("FacetValue.as", destinationRoot + "/src/main/flex/com/wordnik/client.model", "FacetValue.as"),

    ("build.properties", destinationRoot, "build.properties"),
    ("build.xml", destinationRoot, "build.xml"),

    ("ASAXB-0.1.1.swc", destinationRoot + "/lib", "ASAXB-0.1.1.swc")


  )
}

object WordnikApiFlashCodegenRunner {
  def main(args: Array[String]) {
    WordnikApiFlashCodegen.main(args)
  }
}