import com.wordnik.swagger.codegen.BasicObjcGenerator

object ObjcXappServerCodegen extends BasicObjcGenerator {
  def main(args: Array[String]) = generateClient(args)

  // where to write generated code
  override def destinationDir = "samples/client/xapp-server/objc/client"

  // to avoid recompiling ...
  override def templateDir = "src/main/resources/objc"

  // supporting classes
  override def supportingFiles =
    List(
      ("NIKSwaggerObject.h", destinationDir, "NIKSwaggerObject.h"),
      ("NIKSwaggerObject.m", destinationDir, "NIKSwaggerObject.m"),
      ("NIKApiInvoker.h", destinationDir, "NIKApiInvoker.h"),
      ("NIKApiInvoker.m", destinationDir, "NIKApiInvoker.m"),
      ("NIKDate.h", destinationDir, "NIKDate.h"),
      ("NIKDate.m", destinationDir, "NIKDate.m"))

  override def typeMapping = Map(
      "Date" -> "NIKDate",
      "boolean" -> "NSNumber",
      "string" -> "NSString",
      "integer" -> "NSNumber",
      "int" -> "NSNumber",
      "float" -> "NSNumber",
      "long" -> "NSNumber",
      "double" -> "NSNumber",
      "Array" -> "NSArray",
      "List" -> "NSArray",
      "object" -> "NSObject",
      "Number" -> "NSDecimalNumber")
}
