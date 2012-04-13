package com.wordnik.swagger.codegen

import com.wordnik.swagger.core._

import com.wordnik.swagger.codegen.util.CoreUtils
import com.wordnik.swagger.codegen.language.CodegenConfig

import org.fusesource.scalate._
import org.fusesource.scalate.layout.DefaultLayoutStrategy
import org.fusesource.scalate.mustache._

import java.io.File
import java.io.FileWriter

import scala.io.Source
import scala.collection.mutable.{ HashMap, ListBuffer, HashSet }
import scala.collection.JavaConversions._

class Codegen(config: CodegenConfig) {
  val m = com.wordnik.swagger.core.util.JsonUtil.getJsonMapper

  def writeApis(classname: String, basePath: String, allModels: Map[String, DocumentationSchema], operations: List[(String, DocumentationOperation)]) = {
    val data: HashMap[String, AnyRef] =
      HashMap(
        "classname" -> classname,
        "package" -> config.packageName,
        "basePath" -> basePath,
        "newline" -> "\n")

    val modelNames = new HashSet[String]
    operations.map(a => {
      modelNames ++= CoreUtils.extractModelNames(allModels.toMap, a._2)
      modelNames --= config.primitiveTypes
    })
      
    val l = new ListBuffer[AnyRef]
    operations.map(operationMap => {
      val path = operationMap._1
      val op = operationMap._2

      var bodyParam: Option[String] = None

      var queryParams = new ListBuffer[AnyRef]
      val pathParams = new ListBuffer[AnyRef]
      val headerParams = new ListBuffer[AnyRef]
      var paramList = new ListBuffer[HashMap[String, AnyRef]]

      if (op.getParameters != null) {
        op.getParameters.foreach(param => {
          val params = new HashMap[String, AnyRef]
          params += "type" -> param.paramType
          params += "defaultValue" -> config.toDefaultValue(param.dataType, param.defaultValue)
          params += "dataType" -> config.toDeclaredType(param.dataType)
          params += "hasMore" -> "true"
          param.paramType match {
            case "body" => {
              params += "paramName" -> "body"
              params += "baseName" -> "body"
              param.required match {
                case true => params += "required" -> "true"
                case _ =>
              }
              bodyParam = Some("body")
            }
            case "path" => {
              params += "paramName" -> config.toVarName(param.name)
              params += "baseName" -> param.name
              params += "required" -> "true"
              pathParams += params.clone
            }
            case "query" => {
              params += "paramName" -> config.toVarName(param.name)
              params += "baseName" -> param.name
              params += "required" -> param.required.toString
              queryParams += params.clone
            }
            case "header" => {
              params += "paramName" -> config.toVarName(param.name)
              params += "baseName" -> param.name
              params += "required" -> param.required.toString
              headerParams += params.clone
            }
          }
          paramList += params
        })
      }

      val requiredParams = new ListBuffer[HashMap[String, AnyRef]]
      paramList.filter(p => p.contains("required") && p("required") == "true").foreach(param => {
        requiredParams += HashMap(
          "paramName" -> param("paramName"),
          "defaultValue" -> param("defaultValue"),
          "baseName" -> param("baseName"),
          "hasMore" -> "true")
      })
      requiredParams.size match {
        case 0 =>
        case _ => requiredParams.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
      }

      queryParams.size match {
        case 0 =>
        case _ => queryParams.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
      }

      val sp = {
        val lb = new ListBuffer[AnyRef]
        paramList.foreach(i => {
          i("defaultValue") match {
            case Some(e) =>
            case None => lb += i
          }
        })
        paramList.foreach(i => {
          i("defaultValue") match {
            case Some(e) => lb += i
            case None =>
          }
        })
        lb.toList
      }

      paramList.size match {
        case 0 =>
        case _ => sp.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
      }

      val properties =
        HashMap[String, AnyRef](
          "path" -> path,
          "nickname" -> config.toMethodName(op.nickname),
          "summary" -> op.summary,
          "notes" -> op.notes,
          "deprecated" -> op.deprecated,
          "bodyParam" -> bodyParam,
          "allParams" -> sp,
          "pathParams" -> pathParams,
          "queryParams" -> queryParams,
          "headerParams" -> headerParams,
          "requiredParams" -> requiredParams,
          "httpMethod" -> op.httpMethod.toUpperCase,
          op.httpMethod.toLowerCase -> "true")
      if (requiredParams.size > 0) properties += "requiredParamCount" -> requiredParams.size.toString
      op.responseClass.indexOf("[") match {
        case -1 => {
          properties += "returnType" -> config.processResponseClass(op.responseClass)
          properties += "returnBaseType" -> config.processResponseClass(op.responseClass)
        }
        case n: Int => {
          val ComplexTypeMatcher = ".*\\[(.*)\\].*".r
          val ComplexTypeMatcher(basePart) = op.responseClass
          properties += "returnType" -> config.processResponseClass(op.responseClass)
          properties += "returnContainer" -> (op.responseClass.substring(0, n))
          properties += "returnBaseType" -> basePart
        }
      }

      l += properties
    })
    data += "vars" -> l
    val imports = new ListBuffer[AnyRef]
    modelNames.foreach(modelName => {
      // apply mapings, packages for generated code
      val qualifiedModel = (config.importMapping.contains(modelName) match {
        case true => config.importMapping(modelName)
        case false => {
          config.modelPackage match {
            case Some(p) => p + "." + modelName
            case None => modelName
          }
        }
      })
      imports += Map("import" -> qualifiedModel)
    })

    data += "imports" -> imports

    val rootDir = new java.io.File(".")
    val engine = new TemplateEngine(Some(rootDir))

    val template = engine.compile(
      TemplateSource.fromText(config.templateDir + "/api.mustache",
        Source.fromFile(config.templateDir + "/api.mustache").mkString))

    val output = engine.layout(config.templateDir + "/api.mustache", template, data.toMap)

    val outputDir = new File(config.destinationDir + File.separator +
      config.packageName.replaceAll("\\.", File.separator))
    outputDir.mkdirs

    val filename = outputDir + File.separator + classname + config.fileSuffix

    val fw = new FileWriter(filename, false)
    fw.write(output + "\n")
    fw.close()
    println("wrote " + filename)
  }

  def writeModels(ops: Map[String, AnyRef]) = {
    val rootDir = new java.io.File(".")
    val engine = new TemplateEngine(Some(rootDir))

    val template = engine.compile(
      TemplateSource.fromText(config.templateDir + "/model.mustache",
        Source.fromFile(config.templateDir + "/model.mustache").mkString))

    for ((k, o) <- ops) {
      val operation = o.asInstanceOf[DocumentationSchema]
      val data: HashMap[String, AnyRef] =
        HashMap("classname" -> k,
          "modelPackage" -> config.modelPackage,
          "newline" -> "\n")

      val l = new ListBuffer[AnyRef]

      val imports = new HashSet[AnyRef]
      operation.properties.map(prop => {
        val obj = prop._2
        val dt = obj.getType.charAt(0).toUpperCase + obj.getType.substring(1)
        if (config.importMapping.contains(dt))
          imports += Map("import" -> config.importMapping(dt))
        val properties =
          HashMap(
            "name" -> config.toVarName(prop._1),
            "baseName" -> prop._1,
            "datatype" -> config.toDeclaration(obj)._1,
            "defaultValue" -> config.toDeclaration(obj)._2,
            "description" -> obj.description,
            "notes" -> obj.notes,
            "required" -> obj.required.toString,
            "getter" -> config.toGetter(config.toVarName(prop._1), config.toDeclaration(obj)._1),
            "setter" -> config.toSetter(config.toVarName(prop._1), config.toDeclaration(obj)._1),
            "hasMore" -> "true")
        l += properties
      })
      l.size match {
        case 0 =>
        case _ => l.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
      }
      data += "vars" -> l
      data += "imports" -> imports

      val shouldProcess = config.modelsToProcess.size match {
        case 0 => true
        case _ => config.modelsToProcess.contains(k)
      }

      if (shouldProcess) {
        val output = engine.layout(config.templateDir + "/model.mustache", template, data.toMap)
        val outputDir = new File(config.destinationDir + File.separator +
          config.modelPackage.get.replaceAll("\\.", File.separator))
        outputDir.mkdirs

        val filename = outputDir + File.separator + k + config.fileSuffix
        val fw = new FileWriter(filename, false)
        fw.write(output + "\n")
        fw.close()
        println("wrote " + filename)
      }
    }
  }

  def writeSupportingClasses = {
    val rootDir = new java.io.File(".")
    val engine = new TemplateEngine(Some(rootDir))

    val data: HashMap[String, String] =
      HashMap(
        "package" -> config.packageName)

    config.supportingFiles.map(file => {
      val srcTemplate = file._1
      val outputDir = file._2
      val destFile = file._3

      val template = engine.compile(
        TemplateSource.fromText(config.templateDir + File.separator + srcTemplate,
          Source.fromFile(config.templateDir + File.separator + srcTemplate).mkString))

      val output = engine.layout(config.templateDir + File.separator + srcTemplate, template, data.toMap)

      val outputFolder = new File(outputDir.replaceAll("\\.", File.separator))
      outputFolder.mkdirs

      val filename = outputFolder + File.separator + destFile

      val fw = new FileWriter(filename, false)
      fw.write(output + "\n")
      fw.close()
      println("wrote " + filename)
    })

  }
}
