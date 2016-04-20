package io.swagger.client.api

import io.swagger.client.model.Order
import io.swagger.client.ApiInvoker
import io.swagger.client.ApiException

import com.sun.jersey.multipart.FormDataMultiPart
import com.sun.jersey.multipart.file.FileDataBodyPart

import javax.ws.rs.core.MediaType

import java.io.File
import java.util.Date

import scala.collection.mutable.HashMap

class StoreApi(val defBasePath: String = "http://petstore.swagger.io/v2",
                        defApiInvoker: ApiInvoker = ApiInvoker) {
  var basePath = defBasePath
  var apiInvoker = defApiInvoker

  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value 

  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted 
   * @return void
   */
  def deleteOrder (orderId: String)  = {
    // create path and map variables
    val path = "/store/order/{orderId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "orderId" + "\\}",apiInvoker.escape(orderId))


    val contentTypes = List("application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

<<<<<<< HEAD

        
    
    var postBody: AnyRef = null

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
          }

    try {
      apiInvoker.invokeApi(basePath, path, "DELETE", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
                  case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  /**
   * Finds orders by status
   * A single status value can be provided as a string
   * @param status Status value that needs to be considered for query (optional, default to placed)
   * @return List[Order]
   */
  def findOrdersByStatus (status: String /* = placed */) : Option[List[Order]] = {
    // create path and map variables
    val path = "/store/findByStatus".replaceAll("\\{format\\}","json")
    val contentTypes = List("application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]


    if(String.valueOf(status) != "null") queryParams += "status" -> status.toString
    
    
=======
        
    
>>>>>>> upstream/master
    var postBody: AnyRef = null

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
<<<<<<< HEAD
          }

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           Some(ApiInvoker.deserialize(s, "array", classOf[Order]).asInstanceOf[List[Order]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return Map[String, Integer]
   */
  def getInventory () : Option[Map[String, Integer]] = {
    // create path and map variables
    val path = "/store/inventory".replaceAll("\\{format\\}","json")
    val contentTypes = List("application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]


        
    
    var postBody: AnyRef = null

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
=======
>>>>>>> upstream/master
    }
    else {
          }

    try {
      apiInvoker.invokeApi(basePath, path, "DELETE", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
<<<<<<< HEAD
           Some(ApiInvoker.deserialize(s, "map", classOf[Integer]).asInstanceOf[Map[String, Integer]])
        case _ => None
=======
                  case _ => None
>>>>>>> upstream/master
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
<<<<<<< HEAD
  /**
   * Fake endpoint to test arbitrary object return by &#39;Get inventory&#39;
   * Returns an arbitrary object which is actually a map of status codes to quantities
   * @return Any
   */
  def getInventoryInObject () : Option[Any] = {
    // create path and map variables
    val path = "/store/inventory?response&#x3D;arbitrary_object".replaceAll("\\{format\\}","json")
=======

  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return Map[String, Integer]
   */
  def getInventory () : Option[Map[String, Integer]] = {
    // create path and map variables
    val path = "/store/inventory".replaceAll("\\{format\\}","json")
>>>>>>> upstream/master
    val contentTypes = List("application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

<<<<<<< HEAD

=======
>>>>>>> upstream/master
        
    
    var postBody: AnyRef = null

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
<<<<<<< HEAD
          }
=======
    }
>>>>>>> upstream/master

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
<<<<<<< HEAD
           Some(ApiInvoker.deserialize(s, "", classOf[Any]).asInstanceOf[Any])
=======
           Some(ApiInvoker.deserialize(s, "map", classOf[Integer]).asInstanceOf[Map[String, Integer]])
>>>>>>> upstream/master
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
<<<<<<< HEAD
=======

>>>>>>> upstream/master
  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched 
   * @return Order
   */
  def getOrderById (orderId: Long) : Option[Order] = {
    // create path and map variables
    val path = "/store/order/{orderId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "orderId" + "\\}",apiInvoker.escape(orderId))


    val contentTypes = List("application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

<<<<<<< HEAD

=======
>>>>>>> upstream/master
        
    
    var postBody: AnyRef = null

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
<<<<<<< HEAD
          }
=======
    }
>>>>>>> upstream/master

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           Some(ApiInvoker.deserialize(s, "", classOf[Order]).asInstanceOf[Order])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
<<<<<<< HEAD
  /**
   * Place an order for a pet
   * 
   * @param body order placed for purchasing the pet (optional)
=======

  /**
   * Place an order for a pet
   * 
   * @param body order placed for purchasing the pet 
>>>>>>> upstream/master
   * @return Order
   */
  def placeOrder (body: Order) : Option[Order] = {
    // create path and map variables
    val path = "/store/order".replaceAll("\\{format\\}","json")
    val contentTypes = List("application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

<<<<<<< HEAD
=======
    if (body == null) throw new Exception("Missing required parameter 'body' when calling StoreApi->placeOrder")
>>>>>>> upstream/master

        
    
    var postBody: AnyRef = body

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
<<<<<<< HEAD
          }
=======
    }
>>>>>>> upstream/master

    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           Some(ApiInvoker.deserialize(s, "", classOf[Order]).asInstanceOf[Order])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
<<<<<<< HEAD
=======

>>>>>>> upstream/master
}
