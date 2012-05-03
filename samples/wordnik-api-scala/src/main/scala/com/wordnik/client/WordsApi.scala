package com.wordnik.client

import com.wordnik.client.ApiInvoker
import com.wordnik.client.ApiException

import java.util.Date
import com.wordnik.client.model.Definition
import com.wordnik.client.model.ContentProvider
import com.wordnik.client.model.Example
import com.wordnik.client.model.WordObject
import com.wordnik.client.model.WordSearchResult
import com.wordnik.client.model.WordOfTheDay
import com.wordnik.client.model.WordFrequency
import com.wordnik.client.model.WordSearchResults
import scala.collection.mutable.HashMap

class WordsApi {
  var basePath: String = "http://api.wordnik.com/v4"
  var apiInvoker = ApiInvoker

  def searchWords (query: String, includePartOfSpeech: String, excludePartOfSpeech: String, maxCorpusCount: String, maxDictionaryCount: String, maxLength: String, caseSensitive: String= "true", minCorpusCount: String= "5", minDictionaryCount: String= "1", minLength: String= "1", skip: String= "0", limit: String= "10") : Option[List[WordFrequency]]= {
    // create path and map variables
    val path = "/words.{format}/search".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(query) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(query) != "null") queryParams += "query" -> query.toString
    if(String.valueOf(caseSensitive) != "null") queryParams += "caseSensitive" -> caseSensitive.toString
    if(String.valueOf(includePartOfSpeech) != "null") queryParams += "includePartOfSpeech" -> includePartOfSpeech.toString
    if(String.valueOf(excludePartOfSpeech) != "null") queryParams += "excludePartOfSpeech" -> excludePartOfSpeech.toString
    if(String.valueOf(minCorpusCount) != "null") queryParams += "minCorpusCount" -> minCorpusCount.toString
    if(String.valueOf(maxCorpusCount) != "null") queryParams += "maxCorpusCount" -> maxCorpusCount.toString
    if(String.valueOf(minDictionaryCount) != "null") queryParams += "minDictionaryCount" -> minDictionaryCount.toString
    if(String.valueOf(maxDictionaryCount) != "null") queryParams += "maxDictionaryCount" -> maxDictionaryCount.toString
    if(String.valueOf(minLength) != "null") queryParams += "minLength" -> minLength.toString
    if(String.valueOf(maxLength) != "null") queryParams += "maxLength" -> maxLength.toString
    if(String.valueOf(skip) != "null") queryParams += "skip" -> skip.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[WordFrequency]).asInstanceOf[List[WordFrequency]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getWordOfTheDay (date: String, category: String, creator: String) : Option[WordOfTheDay]= {
    // create path and map variables
    val path = "/words.{format}/wordOfTheDay".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    if(String.valueOf(date) != "null") queryParams += "date" -> date.toString
    if(String.valueOf(category) != "null") queryParams += "category" -> category.toString
    if(String.valueOf(creator) != "null") queryParams += "creator" -> creator.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[WordOfTheDay]).asInstanceOf[WordOfTheDay])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def searchWordsNew (query: String, includePartOfSpeech: String, excludePartOfSpeech: String, maxCorpusCount: String, maxDictionaryCount: String, maxLength: String, caseSensitive: String= "true", minCorpusCount: String= "5", minDictionaryCount: String= "1", minLength: String= "1", skip: String= "0", limit: String= "10") : Option[WordSearchResults]= {
    // create path and map variables
    val path = "/words.{format}/search/{query}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "query" + "\\}",apiInvoker.escapeString(query))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(query) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(caseSensitive) != "null") queryParams += "caseSensitive" -> caseSensitive.toString
    if(String.valueOf(includePartOfSpeech) != "null") queryParams += "includePartOfSpeech" -> includePartOfSpeech.toString
    if(String.valueOf(excludePartOfSpeech) != "null") queryParams += "excludePartOfSpeech" -> excludePartOfSpeech.toString
    if(String.valueOf(minCorpusCount) != "null") queryParams += "minCorpusCount" -> minCorpusCount.toString
    if(String.valueOf(maxCorpusCount) != "null") queryParams += "maxCorpusCount" -> maxCorpusCount.toString
    if(String.valueOf(minDictionaryCount) != "null") queryParams += "minDictionaryCount" -> minDictionaryCount.toString
    if(String.valueOf(maxDictionaryCount) != "null") queryParams += "maxDictionaryCount" -> maxDictionaryCount.toString
    if(String.valueOf(minLength) != "null") queryParams += "minLength" -> minLength.toString
    if(String.valueOf(maxLength) != "null") queryParams += "maxLength" -> maxLength.toString
    if(String.valueOf(skip) != "null") queryParams += "skip" -> skip.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[WordSearchResults]).asInstanceOf[WordSearchResults])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getRandomWords (hasDictionaryDef: String, includePartOfSpeech: String, excludePartOfSpeech: String, minCorpusCount: String, maxCorpusCount: String, minDictionaryCount: String, maxDictionaryCount: String, minLength: String, maxLength: String, sortBy: String, sortOrder: String, limit: String) : Option[List[WordObject]]= {
    // create path and map variables
    val path = "/words.{format}/randomWords".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    if(String.valueOf(hasDictionaryDef) != "null") queryParams += "hasDictionaryDef" -> hasDictionaryDef.toString
    if(String.valueOf(includePartOfSpeech) != "null") queryParams += "includePartOfSpeech" -> includePartOfSpeech.toString
    if(String.valueOf(excludePartOfSpeech) != "null") queryParams += "excludePartOfSpeech" -> excludePartOfSpeech.toString
    if(String.valueOf(minCorpusCount) != "null") queryParams += "minCorpusCount" -> minCorpusCount.toString
    if(String.valueOf(maxCorpusCount) != "null") queryParams += "maxCorpusCount" -> maxCorpusCount.toString
    if(String.valueOf(minDictionaryCount) != "null") queryParams += "minDictionaryCount" -> minDictionaryCount.toString
    if(String.valueOf(maxDictionaryCount) != "null") queryParams += "maxDictionaryCount" -> maxDictionaryCount.toString
    if(String.valueOf(minLength) != "null") queryParams += "minLength" -> minLength.toString
    if(String.valueOf(maxLength) != "null") queryParams += "maxLength" -> maxLength.toString
    if(String.valueOf(sortBy) != "null") queryParams += "sortBy" -> sortBy.toString
    if(String.valueOf(sortOrder) != "null") queryParams += "sortOrder" -> sortOrder.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[WordObject]).asInstanceOf[List[WordObject]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getRandomWord (includePartOfSpeech: String, excludePartOfSpeech: String, minCorpusCount: String, maxCorpusCount: String, minDictionaryCount: String, maxDictionaryCount: String, minLength: String, maxLength: String, hasDictionaryDef: String= "true") : Option[WordObject]= {
    // create path and map variables
    val path = "/words.{format}/randomWord".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    if(String.valueOf(hasDictionaryDef) != "null") queryParams += "hasDictionaryDef" -> hasDictionaryDef.toString
    if(String.valueOf(includePartOfSpeech) != "null") queryParams += "includePartOfSpeech" -> includePartOfSpeech.toString
    if(String.valueOf(excludePartOfSpeech) != "null") queryParams += "excludePartOfSpeech" -> excludePartOfSpeech.toString
    if(String.valueOf(minCorpusCount) != "null") queryParams += "minCorpusCount" -> minCorpusCount.toString
    if(String.valueOf(maxCorpusCount) != "null") queryParams += "maxCorpusCount" -> maxCorpusCount.toString
    if(String.valueOf(minDictionaryCount) != "null") queryParams += "minDictionaryCount" -> minDictionaryCount.toString
    if(String.valueOf(maxDictionaryCount) != "null") queryParams += "maxDictionaryCount" -> maxDictionaryCount.toString
    if(String.valueOf(minLength) != "null") queryParams += "minLength" -> minLength.toString
    if(String.valueOf(maxLength) != "null") queryParams += "maxLength" -> maxLength.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[WordObject]).asInstanceOf[WordObject])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  }
