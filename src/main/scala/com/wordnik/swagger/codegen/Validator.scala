package com.wordnik.swagger.codegen

import com.wordnik.swagger.codegen._
import com.wordnik.swagger.codegen.util._
import com.wordnik.swagger.core._
import com.wordnik.swagger.core.util.JsonUtil

import scala.io._
import scala.collection.JavaConversions._
import scala.collection.mutable.{ ListBuffer, HashMap, HashSet }
import scala.io.Source

object Validator {
  def m = JsonUtil.getJsonMapper

  def main(args: Array[String]) = {
    val host = args(0)
    val doc = m.readValue(Source.fromURL(host).mkString, classOf[Documentation])
    validate(doc).foreach(error => println(error))
  }

  def validate(doc: Documentation): List[String] = {
    val errors = new ListBuffer[String]

    doc.swaggerVersion match {
      case e: String => println("swagger version: " + e)
      case _ => errors += "invalid swagger version"
    }
    doc.basePath match {
      case e: String => println("basePath: " + e)
      case _ => errors += "invalid base path"
    }
    doc.apiVersion match {
      case e: String => println("api version: " + e)
      case _ => errors += "no api version"
    }

    errors.toList
  }
}