package com.wordnik.swagger.codegen

import com.wordnik.swagger.codegen.language.CodegenConfig
import com.wordnik.swagger.codegen._
import com.wordnik.swagger.codegen.util._
import com.wordnik.swagger.core._
import com.wordnik.swagger.core.util.JsonUtil

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
      if (subDoc.getApis != null) {
        subDoc.getApis.foreach(api => {
          ApiExtractor.extractOperations(doc.basePath, api).map(op =>
            operations += Tuple3(basePath, op._1, op._2))
        })
        operations.map(op => processOperation(op._2,op._3))
        allModels ++= CoreUtils.extractModels(subDoc)
      }
    })

    val apiToOperationMap = generateApiGroups(operations.toList)

    apiToOperationMap.map(a => codegen.writeApis(a._1._1, a._1._2, allModels.toMap, a._2.toList))
    codegen.writeModels(allModels.toMap)
    codegen.writeSupportingClasses
    exit(0)
  }

  /**
   * groups the operations.  You can override this if you don't like the default grouping which is:
   */
  def generateApiGroups(operations: List[(String, // basePath 
    String, // apiPath
    DocumentationOperation)]): // operation
  Map[(String, // className
    String), // basePath
    List[(String, // apiPath
    DocumentationOperation // operation 
  )]] = {
    val output = new HashMap[(String, String), List[(String, DocumentationOperation)]]

    operations.map(op => {
      val basePath = op._1
      val apiPath = op._2
      val operation = op._3

      val classname = apiNameFromPath(apiPath)

      output.contains(classname, basePath) match {
        case true => output += (classname, basePath) -> (output(classname, basePath) ++ List(Tuple2(apiPath, operation)))
        case false => output += (classname, basePath) -> List(Tuple2(apiPath, operation))
      }
    })
    apisToProcess.size match {
      case 0 => output.toMap
      case i: Int => output.filter(k => {
        println("looking at " + k._1._1)
        apisToProcess.contains(k._1._1)
      }).toMap
    }
  }
}