import com.wordnik.swagger.codegen.BasicGenerator

import com.wordnik.swagger.core._

object WordnikApiObjcCodegen extends ObjcCodegen {
  def main(args: Array[String]) = generateClient(args)

  def destinationRoot = "wordnik-api/objc"

  // where to write generated code
  override def destinationDir =  destinationRoot + "/source"
  
  // supporting classes
  override def supportingFiles =
    List(
      ("SwaggerObject.h", destinationDir, "SwaggerObject.h"),
      ("SwaggerObject.m", destinationDir, "SwaggerObject.m"),
      ("ApiInvoker.h", destinationDir, "ApiInvoker.h"),
      ("ApiInvoker.m", destinationDir, "ApiInvoker.m"),
      ("Date.h", destinationDir, "Date.h"),
      ("Date.m", destinationDir, "Date.m"))
}
