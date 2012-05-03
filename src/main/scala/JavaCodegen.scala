import com.wordnik.swagger.codegen.BasicGenerator

import com.wordnik.swagger.core._

object JavaCodegen extends JavaCodegen {
  def main(args: Array[String]) = generateClient(args)
}

class JavaCodegen extends BasicGenerator {
  val typeMapping = Map(
    "Boolean" -> "boolean",
    "Int" -> "int",
    "Float" -> "float",
    "Long" -> "long",
    "Double" -> "double")
  
  // set imports for common datatypes
  override def imports = Map(
    "ArrayList" -> "java.util.*",
    "List" -> "java.util.*",
    "Date" -> "java.util.Date",
    "Array" -> "java.util.*")

  override def packageName = "com.wordnik.client"

  // location of templates
  override def templateDir = "src/main/resources/java"
    
  // template used for models
  override def modelTemplateFile = "model.mustache"

  // template used for models
  override def apiTemplateFile = "api.mustache"

  // where to write generated code
  override def destinationDir = "src/test/java"

  // import/require statements for specific datatypes
  override def importMapping = Map(
      "Date" -> "java.util.Date",
      "Array" -> "java.util.*",
      "ArrayList" -> "java.util.*")

  // package for models
  override def modelPackage = Some("com.wordnik.javaPetstore.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.javaPetstore.api")

  // file suffix
  override def fileSuffix = ".java"

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
        if(dt.substring(0,n) == "Array") {
          "List" + dt.substring(n).replaceAll("\\[", "<").replaceAll("\\]", ">")
        }
        else dt.charAt(0).toUpperCase + dt.substring(1).replaceAll("\\[", "<").replaceAll("\\]", ">")
      }
      case _ => dt
    }
    typeMapping.getOrElse(declaredType, declaredType)
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