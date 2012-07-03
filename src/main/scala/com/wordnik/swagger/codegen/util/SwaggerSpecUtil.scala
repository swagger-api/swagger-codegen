package com.wordnik.swagger.codegen.util

import com.wordnik.swagger.core._

import com.wordnik.swagger.core.util.JsonUtil
import com.wordnik.swagger.codegen.language.CodegenConfig

import java.util.logging.Logger

import scala.collection.mutable.{ HashSet, ListBuffer, HashMap }
import scala.collection.JavaConversions._

object SwaggerSpecUtil {
  val LOGGER = Logger.getLogger(SwaggerSpecUtil.getClass.getName)

  val primitives = List("int", "string", "long", "double", "float", "boolean", "void")
  val containers = List("List", "Map", "Set", "Array")

  /**
   * this is here because models don't have the proper references to types
   */
  def fixModels(config: CodegenConfig, models: Map[String, DocumentationSchema]) = {
    val validModelNames = models.map(_._1).toSet
    LOGGER.finest("all valid models: " + validModelNames)
    for ((name, model) <- models) {
      // id of model
      getUpdatedType(config, validModelNames, model.id) match {
        case Some(updatedType) => {
          LOGGER.finest("updated " + model.id + " to " + updatedType)
          model.id = updatedType
        }
        case None => LOGGER.finest("can't find type for " + model.name + ", type " + model.id)
      }
      model.properties.foreach(prop => {
        val subObject = prop._2

        if (containers.contains(subObject.getType)) {
          // process the sub object
          if (subObject.items != null && subObject.items.ref != null) {
            getUpdatedType(config, validModelNames, subObject.items.ref) match {
              case Some(updatedType) => {
                LOGGER.finest("updated subObject.items.ref " + subObject.items.ref + " to " + updatedType)
                subObject.items.ref = updatedType
              }
              case None =>
            }
          }
        } else if (containers.contains(toProperCase(subObject.getType))) {
          subObject.setType(toProperCase(subObject.getType))
          // process the sub object
          if (subObject.items != null && subObject.items.ref != null) {
            getUpdatedType(config, validModelNames, subObject.items.ref) match {
              case Some(updatedType) => {
                LOGGER.finest("updated subObject.items.ref " + subObject.items.ref + " to " + updatedType)
                subObject.items.ref = updatedType
              }
              case None => LOGGER.finest("didn't know what to do with " + subObject.items.ref)
            }
          } else if (subObject.items != null && subObject.items.getType != null) {
            getUpdatedType(config, validModelNames, subObject.items.getType) match {
              case Some(updatedType) => {
                LOGGER.finest("updated subObject.items.type" + subObject.items.getType + " to " + updatedType)
                subObject.items.setType(updatedType)
              }
              case None => LOGGER.finest("didn't know what to do with " + subObject.items.ref)
            }
          }
        } else {
          getUpdatedType(config, validModelNames, subObject.getType) match {
            case Some(updatedType) => {
              LOGGER.finest("updated subObject.getType " + subObject.getType + " to " + updatedType)
              subObject.setType(updatedType)
            }
            case None =>
          }
        }
      })
      // remove params with invalid names (Pos???)
      model.properties = model.properties.filter(prop => {
        if (prop._1.indexOf("$") == -1) true
        else {
          LOGGER.finest("removing invalid property " + prop._1)
          false
        }
      })
    }
  }

  /**
   * this is here because sub documents don't have the same resourcePath as declared in
   * the main resource listing
   */
  def fixSubDocs(baseDoc: Documentation, subDoc: Documentation) = {
    if (subDoc.resourcePath.indexOf(".{format}") == -1) {
      baseDoc.getApis.foreach(api => {
        if (api.path.indexOf(".{format}") > 0 && api.path.replaceAll(".\\{format\\}", "") == subDoc.resourcePath) {
          LOGGER.finest("--> added subdoc format string to " + subDoc.resourcePath)
          subDoc.resourcePath = subDoc.resourcePath + ".{format}"
        }
      })
    }
  }

  /**
   * this is here because input params in operations don't match primitives or model names
   */
  def fixInputDataTypes(config: CodegenConfig, models: Map[String, DocumentationSchema], apis: List[DocumentationEndPoint]) = {
    val validModelNames = models.map(m => m._1).toSet
    apis.foreach(api => {
      if (api.getOperations != null) {
        api.getOperations.foreach(op => {
          if (op.getParameters != null) {
            val modelNames = new ListBuffer[String]
            op.getParameters.foreach(p => {

              val dataType = p.dataType
              p.paramType match {
                case "body" => {
                  getUpdatedType(config, validModelNames, dataType) match {
                    case Some(updatedName) => {
//                      LOGGER.finest("--> updated " + dataType + " to " + updatedName)
                      p.dataType = updatedName
                    }
                    case _ => LOGGER.finest("rats!") // leave it alone
                  }
                }
                case "path" => {
                  getUpdatedType(config, validModelNames, dataType) match {
                    case Some(updatedName) => {
//                      LOGGER.finest("--> updated " + dataType + " to " + updatedName)
                      p.dataType = updatedName
                    }
                    case _ => // leave it alone
                  }
                }
                case "query" => {
                  getUpdatedType(config, validModelNames, dataType) match {
                    case Some(updatedName) => {
//                      LOGGER.finest("--> updated " + dataType + " to " + updatedName)
                      p.dataType = updatedName
                    }
                    case _ => // leave it alone
                  }
                }
                case _ =>
              }
            })
          }
        })
      }
    })
  }

  /**
   * this is here because the return types are inconsistent from the swagger-core-1.02-SNAPSHOT
   */
  def fixReturnModels(config: CodegenConfig, models: Map[String, DocumentationSchema], doc: Documentation) = {
    val validModelNames = models.map(m => m._1).toSet

    if (doc.getApis != null) {
      doc.getApis.foreach(api => {
        if (api.getOperations != null) {
          api.getOperations.foreach(op => {
            // check return type
            val responseClass = op.responseClass
            if (responseClass != null) {
              getUpdatedType(config, validModelNames, responseClass) match {
                case Some(updatedName) => {
                  LOGGER.finest("--> updated " + responseClass + " to " + updatedName)
                  op.responseClass = updatedName
                }
                case _ => // leave it alone
              }
            }
          })
        }
      })
    }
  }

  private def getUpdatedType(config: CodegenConfig, validModelNames: Set[String], name: String): Option[String] = {
    if (validModelNames.contains(name)) Some(name)
    else if (name.indexOf("[") > 0) {
      // it's a complex value
      val ComplexTypeMatcher = ".*\\[(.*)\\].*".r
      val ComplexTypeMatcher(basePart) = name

      getUpdatedType(config, validModelNames, basePart) match {
        case Some(updatedPart) => {
          Some(name.replaceAll(java.util.regex.Pattern.quote(basePart), updatedPart))
        }
        case _ => None
      }
    } else if (name.indexOf(".") > 0) {
      val basePart = name.split("\\.").last
      getUpdatedType(config, validModelNames, basePart) match {
        case Some(updatedPart) => {
          Some(updatedPart)
        }
        case _ => {
          None
        }
      }
    } else if (!primitives.contains(name)) {
      val pc = toProperCase(name)
      if (validModelNames.contains(pc)) {
        Some(pc)
      } else if (pc == "Ok") {
        Some("void")
      } else if (primitives.contains(pc)) {
        Some(pc)
      } else if (config.importMapping.contains(pc)) {
        Some(pc)
      } else if ("Integer" == pc) {
        Some("int")
      } else {
        LOGGER.finest("? couldn't figure out what to do with " + name)
        None
      }
    } else {
      None
    }
  }

  // TODO this needs to go away
  def toProperCase(str: String): String = {
    str.charAt(0).toUpperCase + str.substring(1)
  }
}