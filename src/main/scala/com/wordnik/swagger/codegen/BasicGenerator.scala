package com.wordnik.swagger.codegen

import com.wordnik.swagger.codegen.language.CodegenConfig
import com.wordnik.swagger.codegen._
import com.wordnik.swagger.codegen.util._
import com.wordnik.swagger.core._
import com.wordnik.swagger.core.util.JsonUtil
import java.io.{ File, FileWriter }

import scala.io._
import scala.collection.JavaConversions._
import scala.collection.mutable.{ ListBuffer, HashMap, HashSet }
import scala.io.Source

class BasicGenerator extends CodegenConfig {
  def imports = Map(
    "List" -> "scala.collection.mutable.ListBuffer",
    "Date" -> "java.util.Date")
  def packageName = "com.wordnik.client"
  def templateDir = "src/main/resources/scala"
  def destinationDir = "generated-code/src/main/scala"
  def fileSuffix = ".scala"

  override def modelPackage = Some("com.wordnik.client.model")
  override def apiPackage = Some("com.wordnik.client.api")

  var codegen = new Codegen(this)
  def m = JsonUtil.getJsonMapper

  def generateClient(args: Array[String]) = {
    val host = args(0)
    val apiKey = {
      if (args.length > 1) Some("?api_key=" + args(1))
      else None
    }
    val doc = {
      try {
        m.readValue(Source.fromURL(host + apiKey.getOrElse("")).mkString, classOf[Documentation])
      } catch {
        case e: Exception => throw new Exception("unable to read from " + host)
      }
    }

    val subDocs = ApiExtractor.extractApiDocs(doc.basePath, doc.getApis().toList, apiKey)
    val models = CoreUtils.extractAllModels(doc)

    // lets get rid of this loop of uglyness
    subDocs.foreach(subDoc => {
      SwaggerSpecUtil.fixSubDocs(doc, subDoc)
      if (subDoc.getModels != null) {
        SwaggerSpecUtil.fixReturnModels(this, subDoc.getModels.toMap, subDoc)
        SwaggerSpecUtil.fixInputDataTypes(this, subDoc.getModels.toMap, subDoc.getApis.toList)
        SwaggerSpecUtil.fixModels(this, subDoc.getModels.toMap)
      }
    })

    val allModels = new HashMap[String, DocumentationSchema]
    val operations = new ListBuffer[(String, String, DocumentationOperation)]

    subDocs.foreach(subDoc => {
      val basePath = subDoc.basePath
      val resourcePath = subDoc.resourcePath
      if (subDoc.getApis != null) {
        subDoc.getApis.foreach(api => {
          for ((apiPath, operation) <- ApiExtractor.extractOperations(doc.basePath, api))
            operations += Tuple3(basePath, apiPath, operation)
        })
        operations.map(op => processOperation(op._2, op._3))
        allModels ++= CoreUtils.extractModels(subDoc)
      }
    })

    val bundleList = new ListBuffer[Map[String, AnyRef]]
    val apiMap = groupApisToFiles(operations.toList)
    for ((identifier, operationList) <- apiMap) {
      val basePath = identifier._1
      val className = identifier._2
      val map = new HashMap[String, AnyRef]
      map += "basePath" -> basePath
      map += "apis" -> Map(className -> operationList.toList)
      map += "models" -> None
      map += "package" -> apiPackage
      map += "outputDirectory" -> (destinationDir + File.separator + apiPackage.getOrElse("").replaceAll("\\.", File.separator))
      map += "filename" -> (className + fileSuffix)
      bundleList += map.toMap
    }
    for ((name, schema) <- allModels) {
      val map = new HashMap[String, AnyRef]
      map += "apis" -> None
      map += "models" -> List((name, schema))
      map += "package" -> modelPackage
      map += "outputDirectory" -> (destinationDir + File.separator + modelPackage.getOrElse("").replaceAll("\\.", File.separator))
      map += "filename" -> (name + fileSuffix)
      bundleList += map.toMap
    }
    bundleList.foreach(bundle => {
      val output = codegen.generateSource(bundle)
      val outputDir = new File(bundle("outputDirectory").asInstanceOf[String])
      outputDir.mkdirs

      val filename = outputDir + File.separator + bundle("filename")

      val fw = new FileWriter(filename, false)
      fw.write(output + "\n")
      fw.close()
      println("wrote " + filename)
    })
    codegen.writeSupportingClasses
    exit(0)
  }

  def groupApisToFiles(operations: List[(String /*basePath*/ , String /*apiPath*/ , DocumentationOperation /* operation*/ )]): Map[(String, String), ListBuffer[(String, DocumentationOperation)]] = {
    val opMap = new HashMap[(String, String), ListBuffer[(String, DocumentationOperation)]]
    for ((basePath, apiPath, operation) <- operations) {
      val className = apiNameFromPath(apiPath)
      val listToAddTo = opMap.getOrElse((basePath, className), {
        val l = new ListBuffer[(String, DocumentationOperation)]
        opMap += (basePath, className) -> l
        l
      })
      listToAddTo += Tuple2(apiPath, operation)
    }
    opMap.toMap
  }
}