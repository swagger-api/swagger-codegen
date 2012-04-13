package com.wordnik.client

import com.wordnik.client.ApiInvoker
import com.wordnik.client.ApiException

import com.wordnik.client.model.AuthenticationToken
import com.wordnik.client.model.WordList
import com.wordnik.client.model.User
import java.util.Date
import com.wordnik.client.model.ApiTokenStatus
import scala.collection.mutable.HashMap

class AccountApi {
  var basePath: String = "http://api.wordnik.com/v4"
  var apiInvoker = ApiInvoker

  def authenticate (username: String, password: String) : Option[AuthenticationToken]= {
    // create path and map variables
    val path = "/account.{format}/authenticate/{username}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "username" + "\\}",apiInvoker.escapeString(username))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(username, password) - null).size match {
       case 2 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(password) != "null") queryParams += "password" -> password.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[AuthenticationToken]).asInstanceOf[AuthenticationToken])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def authenticatePost (username: String, body: String) : Option[AuthenticationToken]= {
    // create path and map variables
    val path = "/account.{format}/authenticate/{username}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "username" + "\\}",apiInvoker.escapeString(username))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(username, body) - null).size match {
       case 2 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, body, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[AuthenticationToken]).asInstanceOf[AuthenticationToken])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getWordListsForCurrentUser (auth_token: String, skip: Int, limit: Int) : Option[List[WordList]]= {
    // create path and map variables
    val path = "/account.{format}/wordLists".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(auth_token) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(skip) != "null") queryParams += "skip" -> skip.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    headerParams += "auth_token" -> auth_token
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[WordList]).asInstanceOf[List[WordList]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getApiTokenStatus (api_key: String) : Option[ApiTokenStatus]= {
    // create path and map variables
    val path = "/account.{format}/apiTokenStatus".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    headerParams += "api_key" -> api_key
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[ApiTokenStatus]).asInstanceOf[ApiTokenStatus])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getLoggedInUser (api_key: String, auth_token: String) : Option[User]= {
    // create path and map variables
    val path = "/account.{format}/user".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(api_key, auth_token) - null).size match {
       case 2 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    headerParams += "api_key" -> api_key
    headerParams += "auth_token" -> auth_token
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[User]).asInstanceOf[User])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  }
