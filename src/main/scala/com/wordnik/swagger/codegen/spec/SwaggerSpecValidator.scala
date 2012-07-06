package com.wordnik.swagger.codegen.spec

import com.wordnik.swagger.core._

import com.wordnik.swagger.codegen.language.CodegenConfig
import com.wordnik.swagger.codegen.spec.SwaggerSpec._

import java.util.logging.Logger
import String.format

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._

class SwaggerSpecValidator(private val config: CodegenConfig,
                           private val doc: Documentation,
                           private val subDocs: List[Documentation],
                           private val fix: Boolean = true) {
  import ValidationMessage._
  private val validationMessages = ListBuffer.empty[ValidationMessage]

  private val LOGGER = Logger.getLogger(classOf[SwaggerSpecValidator].getName)

  def validate() {
    subDocs.foreach(subDoc => {
      fixSubDocs(doc, subDoc)

      if (subDoc.getModels != null) {
        fixReturnModels(subDoc.getModels.toMap, subDoc)
        fixInputDataTypes(subDoc.getModels.toMap, subDoc.getApis.toList)
        fixModels(subDoc.getModels.toMap)
      }
    })

    println("----------")
    println(this)
  }

  /**
   * this is here because sub documents don't have the same resourcePath as declared in
   * the main resource listing
   */
  private def fixSubDocs(baseDoc: Documentation, subDoc: Documentation) = {
    if (subDoc.resourcePath.indexOf(".{format}") == -1) {
      baseDoc.getApis.foreach(api => {
        if (api.path.indexOf(".{format}") > 0 && api.path.replaceAll(".\\{format\\}", "") == subDoc.resourcePath) {
          LOGGER.finest("--> added subdoc format string to " + subDoc.resourcePath)
          !!(subDoc, format("Resource Path %s - must be %s.{format}", subDoc.resourcePath, subDoc.resourcePath))
          if(fix) subDoc.resourcePath = subDoc.resourcePath + ".{format}"
        }
      })
    }
  }

  /**
   * this is here because models don't have the proper references to types
   */
  private def fixModels(models: Map[String, DocumentationSchema]) = {
    val validModelNames = models.map(_._1).toSet
    LOGGER.finest("all valid models: " + validModelNames)
    for ((name, model) <- models) {
      // id of model
      getUpdatedType(validModelNames, model.id) match {
        case Some(updatedType) => {
          if(!model.id.equals(updatedType)) {
            !!(model, format("Model %s - id appears to be incorrect. Best guess: %s", model.id, updatedType))
            LOGGER.finest("updated " + model.id + " to " + updatedType)
            if(fix) model.id = updatedType
          }
        }
        case None => {
          LOGGER.finest("can't find type for " + model.name + ", type " + model.id)
          !!(model, format("Model %s's type (%s) appears to be missing", model.name, model.id))
        }
      }


      model.properties.foreach(prop => {
        val subObjectName = prop._1
        val subObject = prop._2

        if (containers.contains(subObject.getType)) {
          // process the sub object
          if (subObject.items != null && subObject.items.ref != null) {
            getUpdatedType(validModelNames, subObject.items.ref) match {
              case Some(updatedType) => {
                if(!subObject.items.ref.equals(updatedType)) {
                  !!(model, format("Model %s.%s: %s - ref (%s) appears to be incorrect. Best guess: %s", model.id, subObjectName, subObject.getType, subObject.items.ref, updatedType))
                  LOGGER.finest("updated subObject.items.ref " + subObject.items.ref + " to " + updatedType)
                  if(fix) subObject.items.ref = updatedType
                }
              }
              case None =>
            }
          }
        } else if (containers.contains(toProperCase(subObject.getType))) {
          subObject.setType(toProperCase(subObject.getType))
          // process the sub object
          if (subObject.items != null && subObject.items.ref != null) {
            getUpdatedType(validModelNames, subObject.items.ref) match {
              case Some(updatedType) => {
                if(!subObject.items.ref.equals(updatedType)) {
                  !!(model, format("Model %s.%s: %s - ref (%s) appears to be incorrect. Best guess: %s", model.id, subObjectName, subObject.getType, subObject.items.ref, updatedType))
                  LOGGER.finest("updated subObject.items.ref " + subObject.items.ref + " to " + updatedType)
                  if(fix) subObject.items.ref = updatedType
                }
              }
              case None => {
                !!(model, format("Model %s.%s: %s - ref (%s) appears to be incorrect.", model.id, subObjectName, subObject.getType, subObject.items.ref), ERROR)
                LOGGER.finest("didn't know what to do with " + subObject.items.ref)
              }
            }
          } else if (subObject.items != null && subObject.items.getType != null) {
            getUpdatedType(validModelNames, subObject.items.getType) match {
              case Some(updatedType) => {
                if(!subObject.items.getType.equals(updatedType)) {
                  !!(model, format("Model %s.%s: %s - type (%s) appears to be incorrect. Best guess: %s", model.id, subObjectName, subObject.getType, subObject.items.getType, updatedType))
                  LOGGER.finest("updated subObject.items.type" + subObject.items.getType + " to " + updatedType)
                  if(fix) subObject.items.setType(updatedType)
                }
              }
              case None => {
                !!(model, format("Model %s.%s: %s - ref (%s) appears to be incorrect.", model.id, subObjectName, subObject.getType, subObject.items.ref), ERROR)
                LOGGER.finest("didn't know what to do with " + subObject.items.ref)
              }
            }
          }
        } else {
          getUpdatedType(validModelNames, subObject.getType) match {
            case Some(updatedType) => {
              if(!subObject.getType.equals(updatedType)) {
                !!(model, format("Model %s.%s: %s - type (%s) appears to be incorrect. Best guess: %s", model.id, subObjectName, subObject.getType, subObject.getType, updatedType))
                LOGGER.finest("updated subObject.getType " + subObject.getType + " to " + updatedType)
                if(fix) subObject.setType(updatedType)
              }
            }
            case None =>
          }
        }
      })
      // remove params with invalid names (Pos???)
      model.properties = model.properties.filter(prop => {
        if (prop._1.indexOf("$") == -1) true
        else {
          !!(model, format("Model %s.%s property appears to be incorrect. Removing it", model.id, prop._1))
          LOGGER.finest("removing invalid property " + prop._1)
          if(fix) false else true
        }
      })
    }
  }


  /**
   * this is here because input params in operations don't match primitives or model names
   */
  private def fixInputDataTypes(models: Map[String, DocumentationSchema], apis: List[DocumentationEndPoint]) = {
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
                  getUpdatedType(validModelNames, dataType) match {
                    case Some(updatedName) => {
                      if (!p.dataType.equals(updatedName)) {
                        //                      LOGGER.finest("--> updated " + dataType + " to " + updatedName)
                        !!(p, format("Parameter %s.%s(body: %s) - The data type %s appears to be incorrect. Best guess: %s", config.apiNameFromPath(api.getPath()), op.nickname, p.dataType, p.dataType, updatedName))
                        if(fix) p.dataType = updatedName
                      }
                    }
                    case _ => LOGGER.finest("rats!") // leave it alone
                  }
                }
                case "path" => {
                  getUpdatedType(validModelNames, dataType) match {
                    case Some(updatedName) => {
                      //                      LOGGER.finest("--> updated " + dataType + " to " + updatedName)
                      !!(p, format("Parameter %s.%s(path_%s: %s) - The data type %s appears to be incorrect. Best guess: %s", config.apiNameFromPath(api.getPath()), op.nickname, p.name, p.dataType, p.dataType, updatedName))
                      if(fix) p.dataType = updatedName
                    }
                    case _ => // leave it alone
                  }
                }
                case "query" => {
                  getUpdatedType(validModelNames, dataType) match {
                    case Some(updatedName) => {
                      //                      LOGGER.finest("--> updated " + dataType + " to " + updatedName)
                      !!(p, format("Parameter %s.%s(query_%s: %s) - The data type %s appears to be incorrect. Best guess: %s", config.apiNameFromPath(api.getPath()), op.nickname, p.name, p.dataType, p.dataType, updatedName))
                      if(fix) p.dataType = updatedName
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
  private def fixReturnModels(models: Map[String, DocumentationSchema], doc: Documentation) = {
    val validModelNames = models.map(m => m._1).toSet

    if (doc.getApis != null) {
      doc.getApis.foreach(api => {
        if (api.getOperations != null) {
          api.getOperations.foreach(op => {
            // check return type
            val responseClass = op.responseClass
            if (responseClass != null) {
              getUpdatedType(validModelNames, responseClass) match {
                case Some(updatedName) => {
                  if(!responseClass.equals(updatedName)) {
                    LOGGER.finest("--> updated " + responseClass + " to " + updatedName)
                    !!(op, format("Operation %s.%s - The response class %s appears to be incorrect. Best guess: %s", config.apiNameFromPath(api.getPath()), op.nickname, op.responseClass, updatedName))
                    if(fix) op.responseClass = updatedName
                  }
                }
                case _ => // leave it alone
              }
            }
          })
        }
      })
    }
  }

  private def getUpdatedType(validModelNames: Set[String], name: String): Option[String] = {
    if (validModelNames.contains(name)) Some(name)
    else if (name.indexOf("[") > 0) {
      // it's a complex value
      val ComplexTypeMatcher = ".*\\[(.*)\\].*".r
      val ComplexTypeMatcher(basePart) = name

      getUpdatedType(validModelNames, basePart) match {
        case Some(updatedPart) => {
          Some(name.replaceAll(java.util.regex.Pattern.quote(basePart), updatedPart))
        }
        case _ => None
      }
    } else if (name.indexOf(".") > 0) {
      val basePart = name.split("\\.").last
      getUpdatedType(validModelNames, basePart) match {
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
  private def toProperCase(str: String): String = {
    str.charAt(0).toUpperCase + str.substring(1)
  }

  private def !!(element: AnyRef, message: String, level: String = ValidationMessage.WARNING) {
    validationMessages += new ValidationMessage(element, message, level)
  }

  override def toString = {
    val out = new StringBuilder
    for (v <- validationMessages) {
      out.append(v)
      out.append('\n')
    }

    out.toString()
  }
}

class ValidationMessage(val element: AnyRef, val message: String, val level: String = ValidationMessage.WARNING) {
  override def toString = level + ": " + message
}

object ValidationMessage {
  val WARNING = "Warning"
  val ERROR = "Error"
}