import com.wordnik.swagger.codegen.BasicGenerator

import com.wordnik.swagger.core._

object FlashCodegen extends FlashCodegen {
  def main(args: Array[String]) = generateClient(args)
}

class FlashCodegen extends BasicGenerator {
  override def defaultIncludes = Set(
    "Date",
    "double",
    "int",
    "long",
    "float",
    "String",
    "boolean")

  override def typeMapping = Map(
    "boolean" -> "Boolean",
    "string" -> "String",
    "int" -> "Number",
    "float" -> "Number",
    "long" -> "Number",
    "double" -> "Number")

  override def packageName = "com.wordnik.client"

  // location of templates
  override def templateDir = "src/main/resources/flash"

  // template used for models
  modelTemplateFiles += "model.mustache" -> ".as"

  // template used for models
  apiTemplateFiles += "api.mustache" -> ".as"

  // where to write generated code
  override def destinationDir = "src/test/flash"


  // import/require statements for specific datatypes
  override def importMapping = Map(
    "Array" -> "mx.collections.ArrayCollection",
    "List" -> "mx.collections.ArrayCollection")


  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // file suffix
  override def fileSuffix = ".as"

  // response classes
  override def processResponseClass(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None
      case e: String => Some(e)
    }
  }

  override def processResponseDeclaration(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None
      case e: String => Some(e)
    }
  }

  override def toDeclaredType(dt: String): String = {
    val declaredType = dt.indexOf("[") match {
      case -1 => dt
      case n: Int => {
        if (dt.substring(0, n) == "Array") {
          "ArrayCollection"
        } else dt
      }
      case _ => dt
    }
    println("mapping: ", declaredType, typeMapping.getOrElse(declaredType, declaredType))
    typeMapping.getOrElse(declaredType, declaredType)
  }

  override def toDeclaration(obj: DocumentationSchema) = {
    var declaredType = toDeclaredType(obj.getType)

    declaredType match {
      case "Array" => {
        declaredType = "ArrayCollection"
      }
      case e: String => e
    }

    val defaultValue = toDefaultValue(declaredType, obj)
    declaredType match {
      case "List" => "ArrayCollection"
      case _ =>
    }
    (declaredType, defaultValue)
  }

  // default values
  override def toDefaultValue(properCase: String, obj: DocumentationSchema) = {
    properCase match {
      case "Boolean" => "false"
      case "Number" => "0.0"
      case "List" => "new ArrayCollection()"
      case "Array" => "new ArrayCollection()"
      case _ => "null"
    }
  }
}