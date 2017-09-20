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

  def addPet(host: String, body: Pet)  = {
    import body._
    
    
    val path = "/pet"    
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
      resp          <- client.expect[](req)
    } yield resp
  }
  def deletePet(host: String, petId: Long, apiKey: String)  = {
    
    
    
    val path = "/pet/{petId}"
      .replaceAll("\\{" + "petId" + "\\}",escape(petId.toString))
    
    val httpMethod = Method.DELETE
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      Header("api_key", apiKey))
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(host + path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[](req)
    } yield resp
  }
  def findPetsByStatus(host: String, status: List[String]) : Task[List[Pet]] = {
    
    implicit val returnTypeDecoder: EntityDecoder[List[Pet]] = jsonOf[List[Pet]]
    
    val path = "/pet/findByStatus"    
    val httpMethod = Method.GET
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      (status, Some(status)))

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(host + path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[List[Pet]](req)
    } yield resp
  }
  def findPetsByTags(host: String, tags: List[String]) : Task[List[Pet]] = {
    
    implicit val returnTypeDecoder: EntityDecoder[List[Pet]] = jsonOf[List[Pet]]
    
    val path = "/pet/findByTags"    
    val httpMethod = Method.GET
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      (tags, Some(tags)))

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(host + path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[List[Pet]](req)
    } yield resp
  }
  def getPetById(host: String, petId: Long) : Task[Pet] = {
    
    implicit val returnTypeDecoder: EntityDecoder[Pet] = jsonOf[Pet]
    
    val path = "/pet/{petId}"
      .replaceAll("\\{" + "petId" + "\\}",escape(petId.toString))
    
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
      resp          <- client.expect[Pet](req)
    } yield resp
  }
  def updatePet(host: String, body: Pet)  = {
    import body._
    
    
    val path = "/pet"    
    val httpMethod = Method.PUT
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(host + path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType)).withBody(body)
      resp          <- client.expect[](req)
    } yield resp
  }
  def updatePetWithForm(host: String, petId: Long, name: String, status: String)  = {
    
    
    
    val path = "/pet/{petId}"
      .replaceAll("\\{" + "petId" + "\\}",escape(petId.toString))
    
    val httpMethod = Method.POST
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
  def uploadFile(host: String, petId: Long, additionalMetadata: String, file: File) : Task[ApiResponse] = {
    
    implicit val returnTypeDecoder: EntityDecoder[ApiResponse] = jsonOf[ApiResponse]
    
    val path = "/pet/{petId}/uploadImage"
      .replaceAll("\\{" + "petId" + "\\}",escape(petId.toString))
    
    val httpMethod = Method.POST
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(host + path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[ApiResponse](req)
    } yield resp
  }
}

class HttpServiceApi(service: HttpService) {
  val client = Client.fromHttpService(service)

  def escape(value: String): String = URLEncoder.encode(value, "utf-8").replaceAll("\\+", "%20")

  def addPet(body: Pet)  = {
    import body._
    

    val path = "/pet"    
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
      resp          <- client.expect[](req)
    } yield resp
  }
  def deletePet(petId: Long, apiKey: String)  = {
    
    

    val path = "/pet/{petId}"
      .replaceAll("\\{" + "petId" + "\\}",escape(petId.toString))
    
    val httpMethod = Method.DELETE
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      Header("api_key", apiKey))
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[](req)
    } yield resp
  }
  def findPetsByStatus(status: List[String]) : Task[List[Pet]] = {
    
    implicit val returnTypeDecoder: EntityDecoder[List[Pet]] = jsonOf[List[Pet]]

    val path = "/pet/findByStatus"    
    val httpMethod = Method.GET
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      (status, Some(status)))

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[List[Pet]](req)
    } yield resp
  }
  def findPetsByTags(tags: List[String]) : Task[List[Pet]] = {
    
    implicit val returnTypeDecoder: EntityDecoder[List[Pet]] = jsonOf[List[Pet]]

    val path = "/pet/findByTags"    
    val httpMethod = Method.GET
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      (tags, Some(tags)))

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[List[Pet]](req)
    } yield resp
  }
  def getPetById(petId: Long) : Task[Pet] = {
    
    implicit val returnTypeDecoder: EntityDecoder[Pet] = jsonOf[Pet]

    val path = "/pet/{petId}"
      .replaceAll("\\{" + "petId" + "\\}",escape(petId.toString))
    
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
      resp          <- client.expect[Pet](req)
    } yield resp
  }
  def updatePet(body: Pet)  = {
    import body._
    

    val path = "/pet"    
    val httpMethod = Method.PUT
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType)).withBody(body)
      resp          <- client.expect[](req)
    } yield resp
  }
  def updatePetWithForm(petId: Long, name: String, status: String)  = {
    
    

    val path = "/pet/{petId}"
      .replaceAll("\\{" + "petId" + "\\}",escape(petId.toString))
    
    val httpMethod = Method.POST
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
  def uploadFile(petId: Long, additionalMetadata: String, file: File) : Task[ApiResponse] = {
    
    implicit val returnTypeDecoder: EntityDecoder[ApiResponse] = jsonOf[ApiResponse]

    val path = "/pet/{petId}/uploadImage"
      .replaceAll("\\{" + "petId" + "\\}",escape(petId.toString))
    
    val httpMethod = Method.POST
    val contentType = `Content-Type`(MediaType.`application/json`)
    val headers = Headers(
      )
    val queryParams = Query(
      )

    for {
      uri           <- Task.fromDisjunction(Uri.fromString(path))
      uriWithParams =  uri.copy(query = queryParams)
      req           =  Request(method = httpMethod, uri = uriWithParams, headers = headers.put(contentType))
      resp          <- client.expect[ApiResponse](req)
    } yield resp
  }
}

