import com.wordnik.swagger.codegen.BasicGenerator

import com.wordnik.swagger.core._

object ScalaCodegen extends ScalaCodegen {
  def main(args: Array[String]) = generateClient(args)
}

class ScalaCodegen extends BasicGenerator {
  // set imports for common datatypes
  override def imports = Map(
    "List" -> "scala.collection.mutable.ListBuffer",
    "Date" -> "java.util.Date")

  // template used for models
  modelTemplateFiles += "model.mustache" -> ".scala"

  // template used for models
  apiTemplateFiles += "api.mustache" -> ".scala"

  // location of templates
  override def templateDir = "src/main/resources/scala"

  // where to write generated code
  override def destinationDir = "generated-code/scala/src/main/scala"

  // reserved words which need special quoting
  override def reservedWords = Set("type", "package", "match", "object")

  // import/require statements for specific datatypes
  override def importMapping = Map("Date" -> "java.util.Date")

  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

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

  // supporting classes
  override def supportingFiles = List(
    ("apiInvoker.mustache", "generated-code/scala/src/main/scala/com/wordnik/client", "ApiInvoker.scala"),
    ("pom.mustache", "generated-code/scala", "pom.xml"))
}