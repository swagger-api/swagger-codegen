import com.wordnik.swagger.codegen.BasicGenerator

import com.wordnik.swagger.core._

object PythonCodegen extends PythonCodegen {
  def main(args: Array[String]) = generateClient(args)
}

class PythonCodegen extends BasicGenerator {
  // set imports for common datatypes
  override def imports = Map()

  // template used for models
  override def modelTemplateFile = "model.mustache"

  // template used for models
  override def apiTemplateFile = "api.mustache"

  // location of templates
  override def templateDir = "src/main/resources/python"

  // where to write generated code
  override def destinationDir = "generated-code/python"
    
  // package for models
  override def modelPackage = Some("")

  // package for apis
  override def apiPackage = Some("")

  // file suffix
  override def fileSuffix = ".py"

  // reserved words which need special quoting
  override def reservedWords = Set("type", "package", "match", "object")

  // import/require statements for specific datatypes
  override def importMapping = Map()

  // response classes--if you don't want a response class, override and set to None
  override def processResponseClass(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None
      case e: String => Some(e)
    }
  }

  override def toDeclaration(obj: DocumentationSchema) = {
    var datatype = obj.getType.charAt(0).toUpperCase + obj.getType.substring(1)
    datatype match {
      case "Array" => {
        datatype = "List"
      }
      case e: String => e
    }

    val defaultValue = toDefaultValue(datatype, obj)
    datatype match {
      case "List" => {
        val inner = {
          if (obj.items.ref != null) obj.items.ref
          else obj.items.getType
        }
        datatype = "java.util.List[" + inner + "]"
      }
      case _ =>
    }
    (datatype, defaultValue)
  }

  // escape keywords
  override def escapeReservedWord(word: String) = "`" + word + "`"
}