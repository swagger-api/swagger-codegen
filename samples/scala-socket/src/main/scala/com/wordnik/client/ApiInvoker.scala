package com.wordnik.client

import com.wordnik.swaggersocket.client._
import com.wordnik.swagger.core.util.JsonUtil

import com.wordnik.swaggersocket.protocol._
import com.wordnik.swaggersocket.client.{ SwaggerSocketException, SwaggerSocketListener, SwaggerSocket }

import org.slf4j.LoggerFactory

import java.util.concurrent._
import java.net.URLEncoder

import scala.collection.JavaConverters._
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

object ApiInvoker {
  val invokers = new HashMap[String, SocketInvoker]()
  def escapeString(value: String): String = {
    URLEncoder.encode(value, "utf-8").replaceAll("\\+", "%20")
  }

  def invokeApi(host: String, path: String, method: String, queryParams: Map[String, String] = Map(), body: AnyRef, headerParams: Map[String, String]) = {
    println("invoking " + host.toString + ", " + path + ", " + method + ", " + queryParams + ", " + body.toString + ", " + headerParams.toString)
    getInvoker(host).invokeApi(path, method, queryParams, body, headerParams)
  }

  def getInvoker(host: String) = {
    invokers.getOrElse(host, {
      val i = new SocketInvoker(host.replaceAll("http", "ws"))
      ApiInvoker.invokers += host -> i
      i
    })
  }

  def deserialize(json: String, containerType: String, cls: Class[_]) = {
    if (cls == classOf[String]) {
      json
    } else {
      containerType match {
        case "List" => {
          val typeInfo = org.codehaus.jackson.map.`type`.TypeFactory.collectionType(classOf[java.util.List[_]], cls)
          val response = JsonUtil.getJsonMapper.readValue(json, typeInfo).asInstanceOf[java.util.List[_]]
          response.asScala.toList
        }
        case _ => JsonUtil.getJsonMapper.readValue(json, cls)
      }
    }
  }

  def serialize(obj: AnyRef): String = {
    if (obj != null) {
      obj match {
        case e: List[_] => JsonUtil.getJsonMapper.writeValueAsString(obj.asInstanceOf[List[_]].asJava)
        case _ => JsonUtil.getJsonMapper.writeValueAsString(obj)
      }
    } else null
  }
}

class SocketInvoker {
  val logger = LoggerFactory.getLogger(classOf[SocketInvoker])
  var ss = SwaggerSocket()
  var host: String = null
  var latchs: ConcurrentLinkedQueue[CountDownLatch] = new ConcurrentLinkedQueue[CountDownLatch]
  var normalClose = false
  var cleaner: ExecutorService = Executors.newSingleThreadExecutor()

  val defaultHeaders: HashMap[String, String] = HashMap()

  val listener = new SwaggerSocketListener() {
    override def close {
      for (cd <- latchs) cd.countDown
      latchs.clear
      if (!normalClose) {
        logger.trace("Socket closed. Re-opening")
        try {
          ss = SwaggerSocket().open(new Request.Builder().path(host).build()).listener(this)
        } catch {
          case t: Throwable => logger.trace("Re-open exception", t)
        }
      }
    }

    override def error(e: SwaggerSocketException) {
      for (cd <- latchs) {
        cd.countDown
      }
      latchs.clear
      logger.error("Unexpected error {} ", e.getMessage, e)
    }

    override def message(r: Request, s: Response) {
      val cd: CountDownLatch = r.attachment match {
        case l: CountDownLatch => l
        case _ => throw new ClassCastException
      }

      r.attach(s.getMessageBody)
      cd.countDown
    }
  }

  def this(host: String) = {
    this()
    this.host = host
    println("created invoker with host " + host)
    ss = ss.open(new Request.Builder().path(host).build()).listener(listener)
  }

  def invokeApi(path: String, method: String, queryParams: Map[String, String] = Map(), body: AnyRef, headerParams: Map[String, String]) = {
    println("host: " + host)
    val cd: CountDownLatch = new CountDownLatch(1)

    val allHeaders = headerParams ++ defaultHeaders

    val request = new Request.Builder()
      .path(path)
      .method(method.toUpperCase)
      .queryString(queryParams.map(p => new QueryString(p._1, p._2)).toList)
      .headers(allHeaders.map(h => new Header(h._1, h._2)).toList)
      .format("JSON")
      .attach(cd)
      .body({
        body match {
          case data: AnyRef => ApiInvoker.serialize(data)
          case _ => null
        }
      }).build()

    ss.send(request)

    latchs.add(cd)
    try {
      if (!cd.await(4 * 60, TimeUnit.SECONDS)) {
        logger.error("No response after 120 seconds");
        ""
      } else {
        request.attachment.toString
      }
    } finally {

      cleaner.execute(new Runnable {
        override def run {
          latchs.remove(cd)
        }
      })
    }
  }

  def close() {
    cleaner.shutdownNow()
    normalClose = true
    ss.close
  }
}

class ApiException extends Exception {
  var code = 0

  def this(code: Int, msg: String) = {
    this()
  }
}
