/**
 *  Copyright 2014 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.codegen

import com.wordnik.swagger.model._

import java.io.File
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object BasicPHPGenerator extends BasicPHPGenerator {
  def main(args: Array[String]) = generateClient(args)
}

class BasicPHPGenerator extends BasicGenerator {

  override def packageName = ""

  // template used for models
  modelTemplateFiles += "model.mustache" -> ".php"

  // template used for models
  apiTemplateFiles += "api.mustache" -> ".php"

  // location of templates
  override def templateDir = "php"

  // where to write generated code
  override def destinationDir = "generated-code/php"

  override def packageSeparator = "\\"

  // package for models
  override def modelPackage: Option[String] = Some("Model")

  // package for apis
  override def apiPackage: Option[String] = None

  // file suffix
  override def fileSuffix = ".php"

  // reserved words which need special quoting
  // These will all be object properties, in which context we don't need
  // to worry about escaping them for PHP.
  override def reservedWords = Set()

  // import/require statements for specific datatypes
  override def importMapping = Map(
    "DateTime" -> "DateTime"
  )

  override def processResponseDeclaration(responseClass: String): Option[String] = {
    typeMapping.contains(responseClass) match {
      case true => Some(typeMapping(responseClass))
      case false => {
        responseClass match {
          case "void" => None
          case e: String => {
            val found = "^([Aa]rray|List)\\[(.*)\\]$".r.findFirstMatchIn(e)

            found.isEmpty match {
              case true => {
                val importScope = modelPackage match {
                  case Some(s) => s + packageSeparator
                  case _ => ""
                }
                Some(importScope + responseClass)
              }
              case false => {
                val responseSubClass = found.get.group(2)
                Some("array[" + processResponseDeclaration(responseSubClass).getOrElse("") + "]")
              }
            }
          }
        }
      }
    }
  }

  override def typeMapping = Map(
    "string" -> "string",
    "str" -> "string",
    "int" -> "int",
    "float" -> "float",
    "long" -> "int",
    "double" -> "float",
    "Array" -> "array",
    "boolean" -> "bool",
    "Date" -> "DateTime"
    )

  override def toDeclaredType(declaredType: String): String = {
    processResponseDeclaration(declaredType).getOrElse(declaredType)
  }

  override def toDeclaration(obj: ModelProperty) = {
    var declaredType = obj.`type`

    declaredType match {
      case "Array" | "List" => {
        val inner = {
          obj.items match {
            case Some(items) => items.ref.getOrElse(items.`type`)
            case _ => {
              println("failed on " + declaredType + ", " + obj)
              throw new Exception("no inner type defined")
            }
          }
        }
        declaredType += "[" + inner + "]"
      }
      case _ =>
    }
    declaredType = processResponseDeclaration(declaredType).getOrElse(declaredType)

    val defaultValue = toDefaultValue(declaredType, obj)
    (declaredType, defaultValue)
  }

  // escape keywords
  override def escapeReservedWord(word: String) = "`" + word + "`"

  // supporting classes
  override def supportingFiles = List(
    ("Swagger.mustache", destinationDir + File.separator + packageName.replace(packageSeparator, File.separator),
     "Swagger.php")
  )
}
