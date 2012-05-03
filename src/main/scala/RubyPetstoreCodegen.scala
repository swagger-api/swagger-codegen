import com.wordnik.swagger.codegen.BasicGenerator

import com.wordnik.swagger.core._

object RubyPetstoreCodegen extends RubyCodegen {
  def main(args: Array[String]) = generateClient(args)
  
  // where to write generated code
  override def destinationDir = "samples/ruby"

  // package for models
  override def modelPackage = Some("models")
}