import com.wordnik.swagger.codegen.BasicGenerator

object ObjcPetstoreCodegen extends ObjcCodegen {
  def main(args: Array[String]) = generateClient(args)

  // where to write generated code
  override def destinationDir = "samples/objc/"

  // supporting classes
  override def supportingFiles =
    List(
      ("JSONKit.h", destinationDir, "JSONKit.h"),
      ("JSONKit.m", destinationDir, "JSONKit.m"),
      ("Date.h", destinationDir, "Date.h"),
      ("Date.m", destinationDir, "Date.m"))
}
