import com.wordnik.swagger.codegen.BasicGenerator

import com.wordnik.swagger.core._

object ObjcCodegen extends ObjcCodegen {
  def main(args: Array[String]) = generateClient(args)
}

class ObjcCodegen extends BasicGenerator {
  val primitiveTypes = Set("NSInteger")

  override def reservedWords = Set("void", "char", "short", "int", "void", "char", "short", "int", "long", "float", "double", "signed", "unsigned", "id", "const", "volatile", "in", "out", "inout", "bycopy", "byref", "oneway", "self", "super")

  val typeMapping = Map(
    "String" -> "NSString",
    "Int" -> "NSInteger",
    "Float" -> "NSNumber",
    "Long" -> "NSNumber",
    "Double" -> "NSNumber",
    "Array" -> "NSArray")

  // set imports for common datatypes
  override def imports = Map(
    "ArrayList" -> "java.util.*",
    "List" -> "java.util.*",
    "Date" -> "java.util.Date",
    "Array" -> "java.util.*")

  override def packageName = "com.wordnik.client"

  // location of templates
  override def templateDir = "src/main/resources/objc"

  // template used for models
  modelTemplateFiles += "model-header.mustache" -> ".h"
  modelTemplateFiles += "model-body.mustache" -> ".m"

  // template used for models
  //  apiTemplateFiles += "api.mustache" -> ".h"

  // where to write generated code
  override def destinationDir = "generated-code/objc/src/main/objc"

  // package for models
  override def modelPackage = None

  // package for api classes
  override def apiPackage = None

  // response classes
  override def processResponseClass(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None
      case e: String => Some(e.replaceAll("\\[", "<").replaceAll("\\]", ">"))
    }
  }

  override def toDeclaredType(dt: String): String = {
    val declaredType = dt.indexOf("[") match {
      case -1 => dt.charAt(0).toUpperCase + dt.substring(1)
      case n: Int => {
        if (dt.substring(0, n) == "Array") {
          "List" + dt.substring(n).replaceAll("\\[", "<").replaceAll("\\]", ">")
        } else dt.charAt(0).toUpperCase + dt.substring(1).replaceAll("\\[", "<").replaceAll("\\]", ">")
      }
      case _ => dt
    }
    val t = typeMapping.getOrElse(declaredType, declaredType)
    primitiveTypes.contains(t) match {
      case true => t
      case _ => t + "*" // needs pointer
    }
  }

  override def toDeclaration(obj: DocumentationSchema) = {
    var declaredType = toDeclaredType(obj.getType)

    declaredType match {
      case "Array" => {
        declaredType = "List"
      }
      case e: String => e
    }

    val defaultValue = toDefaultValue(declaredType, obj)
    declaredType match {
      case "List" => {
        val inner = {
          if (obj.items.ref != null) obj.items.ref
          else obj.items.getType
        }
        declaredType += "<" + inner + ">"
      }
      case _ =>
    }
    (declaredType, defaultValue)
  }

  override def escapeReservedWord(word: String) = "_" + word

  // default values
  override def toDefaultValue(properCase: String, obj: DocumentationSchema) = {
    properCase match {
      case "boolean" => "false"
      case "int" => "0"
      case "long" => "0L"
      case "float" => "0.0f"
      case "double" => "0.0"
      case "List" => {
        val inner = {
          if (obj.items.ref != null) obj.items.ref
          else obj.items.getType
        }
        "new ArrayList<" + inner + ">" + "()"
      }
      case _ => "null"
    }
  }
}