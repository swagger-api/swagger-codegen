package com.wordnik.client

import com.wordnik.client.ApiInvoker
import com.wordnik.client.ApiException

import com.wordnik.client.model.Frequency
import com.wordnik.client.model.Facet
import java.util.Date
import com.wordnik.client.model.Definition
import com.wordnik.client.model.TextPron
import com.wordnik.client.model.AudioFile
import com.wordnik.client.model.Example
import com.wordnik.client.model.Syllable
import com.wordnik.client.model.PartOfSpeech
import com.wordnik.client.model.ExampleSearchResults
import com.wordnik.client.model.WordObject
import com.wordnik.client.model.Bigram
import com.wordnik.client.model.FrequencySummary
import com.wordnik.client.model.Related
import scala.collection.mutable.HashMap

class WordApi {
  var basePath: String = "http://api.wordnik.com/v4"
  var apiInvoker = ApiInvoker

  def getExamples (word: String, includeDuplicates: String, contentProvider: String, useCanonical: String, skip: String, limit: String) : Option[ExampleSearchResults]= {
    // create path and map variables
    val path = "/word.{format}/{word}/examples".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(includeDuplicates) != "null") queryParams += "includeDuplicates" -> includeDuplicates.toString
    if(String.valueOf(contentProvider) != "null") queryParams += "contentProvider" -> contentProvider.toString
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(skip) != "null") queryParams += "skip" -> skip.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[ExampleSearchResults]).asInstanceOf[ExampleSearchResults])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getWord (word: String, includeSuggestions: String, useCanonical: String= "false") : Option[WordObject]= {
    // create path and map variables
    val path = "/word.{format}/{word}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(includeSuggestions) != "null") queryParams += "includeSuggestions" -> includeSuggestions.toString
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
  def getDefinitions (word: String, limit: String, partOfSpeech: String, sourceDictionaries: String, includeRelated: String= "false", useCanonical: String= "false", includeTags: String= "false") : Option[List[Definition]]= {
    // create path and map variables
    val path = "/word.{format}/{word}/definitions".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    if(String.valueOf(partOfSpeech) != "null") queryParams += "partOfSpeech" -> partOfSpeech.toString
    if(String.valueOf(includeRelated) != "null") queryParams += "includeRelated" -> includeRelated.toString
    if(String.valueOf(sourceDictionaries) != "null") queryParams += "sourceDictionaries" -> sourceDictionaries.toString
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(includeTags) != "null") queryParams += "includeTags" -> includeTags.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[Definition]).asInstanceOf[List[Definition]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getTopExample (word: String, contentProvider: String, useCanonical: String) : Option[Example]= {
    // create path and map variables
    val path = "/word.{format}/{word}/topExample".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(contentProvider) != "null") queryParams += "contentProvider" -> contentProvider.toString
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[Example]).asInstanceOf[Example])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getTextPronunciations (word: String, useCanonical: String, sourceDictionary: String, typeFormat: String, limit: String) : Option[List[TextPron]]= {
    // create path and map variables
    val path = "/word.{format}/{word}/pronunciations".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(sourceDictionary) != "null") queryParams += "sourceDictionary" -> sourceDictionary.toString
    if(String.valueOf(typeFormat) != "null") queryParams += "typeFormat" -> typeFormat.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[TextPron]).asInstanceOf[List[TextPron]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getHyphenation (word: String, useCanonical: String, sourceDictionary: String, limit: String) : Option[List[Syllable]]= {
    // create path and map variables
    val path = "/word.{format}/{word}/hyphenation".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(sourceDictionary) != "null") queryParams += "sourceDictionary" -> sourceDictionary.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[Syllable]).asInstanceOf[List[Syllable]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getWordFrequency (word: String, useCanonical: String, startYear: String, endYear: String) : Option[FrequencySummary]= {
    // create path and map variables
    val path = "/word.{format}/{word}/frequency".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(startYear) != "null") queryParams += "startYear" -> startYear.toString
    if(String.valueOf(endYear) != "null") queryParams += "endYear" -> endYear.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[FrequencySummary]).asInstanceOf[FrequencySummary])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getPhrases (word: String, limit: String, wlmi: String, useCanonical: String) : Option[List[Bigram]]= {
    // create path and map variables
    val path = "/word.{format}/{word}/phrases".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    if(String.valueOf(wlmi) != "null") queryParams += "wlmi" -> wlmi.toString
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[Bigram]).asInstanceOf[List[Bigram]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getRelated (word: String, partOfSpeech: String, sourceDictionary: String, limit: String, useCanonical: String, `type`: String) : Option[List[Related]]= {
    // create path and map variables
    val path = "/word.{format}/{word}/related".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(partOfSpeech) != "null") queryParams += "partOfSpeech" -> partOfSpeech.toString
    if(String.valueOf(sourceDictionary) != "null") queryParams += "sourceDictionary" -> sourceDictionary.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(`type`) != "null") queryParams += "type" -> `type`.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[Related]).asInstanceOf[List[Related]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getAudio (word: String, useCanonical: String, limit: String) : Option[List[AudioFile]]= {
    // create path and map variables
    val path = "/word.{format}/{word}/audio".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}",apiInvoker.escapeString(word))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(word) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(useCanonical) != "null") queryParams += "useCanonical" -> useCanonical.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[AudioFile]).asInstanceOf[List[AudioFile]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  }
