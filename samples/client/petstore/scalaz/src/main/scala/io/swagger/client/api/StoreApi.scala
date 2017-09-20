package io.swagger.client.api

import argonaut._
import argonaut.EncodeJson._
import argonaut.DecodeJson._

import java.net.URLEncoder
import java.util.UUID

import org.http4s._
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.argonaut._
import org.http4s.client._
import org.http4s.client.blaze.PooledHttp1Client
import org.http4s.headers._

import org.joda.time.DateTime

import scalaz.concurrent.Task

object DateTimeCodecs {
  implicit def dateTimeEncodeJson: EncodeJson[DateTime] =
    EncodeJson[DateTime](dt => StringEncodeJson(dt.toString))

  implicit def dateTimeDecodeJson: DecodeJson[DateTime] =
    DecodeJson.of[String].map(DateTime.parse(_)) setName "org.joda.time.DateTime"
}

object DefaultApi {

  val client = PooledHttp1Client()

  def escape(value: String): String = URLEncoder.encode(value, "utf-8").replaceAll("\\+", "%20")

  def deleteOrder(host: String, orderId: String)  = {
    
    
    
    val path = "/store/order/{orderId}"
      .replaceAll("\\{" + "orderId" + "\\}",escape(orderId.toString))
    
    val httpMethod = Method.DELETE
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(host + path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[](req)
    } yield resp
  }
  def getInventory(host: String, ) : Task[Map[String, Integer]] = {
    
    implicit val returnTypeDecoder: EntityDecoder[Map[String, Integer]] = jsonOf[Map[String, Integer]]
    
    val path = "/store/inventory"    
    val httpMethod = Method.GET
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(host + path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[Map[String, Integer]](req)
    } yield resp
  }
  def getOrderById(host: String, orderId: Long) : Task[Order] = {
    
    implicit val returnTypeDecoder: EntityDecoder[Order] = jsonOf[Order]
    
    val path = "/store/order/{orderId}"
      .replaceAll("\\{" + "orderId" + "\\}",escape(orderId.toString))
    
    val httpMethod = Method.GET
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(host + path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[Order](req)
    } yield resp
  }
  def placeOrder(host: String, body: Order) : Task[Order] = {
    import body._
    implicit val returnTypeDecoder: EntityDecoder[Order] = jsonOf[Order]
    
    val path = "/store/order"    
    val httpMethod = Method.POST
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(host + path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType)).withBody(body)
      resp          <- client.expect[Order](req)
    } yield resp
  }
}

class HttpServiceApi(service: HttpService) {
  val client = Client.fromHttpService(service)

  def escape(value: String): String = URLEncoder.encode(value, "utf-8").replaceAll("\\+", "%20")

  def deleteOrder(orderId: String)  = {
    
    

    val path = "/store/order/{orderId}"
      .replaceAll("\\{" + "orderId" + "\\}",escape(orderId.toString))
    
    val httpMethod = Method.DELETE
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[](req)
    } yield resp
  }
  def getInventory() : Task[Map[String, Integer]] = {
    
    implicit val returnTypeDecoder: EntityDecoder[Map[String, Integer]] = jsonOf[Map[String, Integer]]

    val path = "/store/inventory"    
    val httpMethod = Method.GET
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[Map[String, Integer]](req)
    } yield resp
  }
  def getOrderById(orderId: Long) : Task[Order] = {
    
    implicit val returnTypeDecoder: EntityDecoder[Order] = jsonOf[Order]

    val path = "/store/order/{orderId}"
      .replaceAll("\\{" + "orderId" + "\\}",escape(orderId.toString))
    
    val httpMethod = Method.GET
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[Order](req)
    } yield resp
  }
  def placeOrder(body: Order) : Task[Order] = {
    import body._
    implicit val returnTypeDecoder: EntityDecoder[Order] = jsonOf[Order]

    val path = "/store/order"    
    val httpMethod = Method.POST
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType)).withBody(body)
      resp          <- client.expect[Order](req)
    } yield resp
  }
}

