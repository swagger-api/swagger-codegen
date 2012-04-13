package com.wordnik.client;

import com.wordnik.client.ApiException;
import com.wordnik.client.ApiInvoker;
import java.util.Date;
import com.wordnik.client.model.Definition;
import com.wordnik.client.model.ContentProvider;
import com.wordnik.client.model.Example;
import com.wordnik.client.model.WordObject;
import com.wordnik.client.model.WordOfTheDay;
import com.wordnik.client.model.WordFrequency;
import java.util.*;

public class WordsApi {
  String basePath = "http://api.wordnik.com/v4";
  ApiInvoker apiInvoker = ApiInvoker.getInstance();

  public ApiInvoker getInvoker() {
    return apiInvoker;
  }
  
  public void setBasePath(String basePath) {
    this.basePath = basePath;
  }
  
  public String getBasePath() {
    return basePath;
  }

  public List<WordFrequency> searchWords (String query, String includePartOfSpeech, String excludePartOfSpeech, int minCorpusCount, int maxCorpusCount, int minDictionaryCount, int maxDictionaryCount, int minLength, int maxLength, int skip, int limit, String caseSensitive) throws ApiException {
    // create path and map variables
    String path = "/words.{format}/search".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(query == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(query)))
      queryParams.put("query", String.valueOf(query));
    if(!"null".equals(String.valueOf(caseSensitive)))
      queryParams.put("caseSensitive", String.valueOf(caseSensitive));
    if(!"null".equals(String.valueOf(includePartOfSpeech)))
      queryParams.put("includePartOfSpeech", String.valueOf(includePartOfSpeech));
    if(!"null".equals(String.valueOf(excludePartOfSpeech)))
      queryParams.put("excludePartOfSpeech", String.valueOf(excludePartOfSpeech));
    if(!"null".equals(String.valueOf(minCorpusCount)))
      queryParams.put("minCorpusCount", String.valueOf(minCorpusCount));
    if(!"null".equals(String.valueOf(maxCorpusCount)))
      queryParams.put("maxCorpusCount", String.valueOf(maxCorpusCount));
    if(!"null".equals(String.valueOf(minDictionaryCount)))
      queryParams.put("minDictionaryCount", String.valueOf(minDictionaryCount));
    if(!"null".equals(String.valueOf(maxDictionaryCount)))
      queryParams.put("maxDictionaryCount", String.valueOf(maxDictionaryCount));
    if(!"null".equals(String.valueOf(minLength)))
      queryParams.put("minLength", String.valueOf(minLength));
    if(!"null".equals(String.valueOf(maxLength)))
      queryParams.put("maxLength", String.valueOf(maxLength));
    if(!"null".equals(String.valueOf(skip)))
      queryParams.put("skip", String.valueOf(skip));
    if(!"null".equals(String.valueOf(limit)))
      queryParams.put("limit", String.valueOf(limit));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (List<WordFrequency>) ApiInvoker.deserialize(response, "List", WordFrequency.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  public WordOfTheDay getWordOfTheDay (String date) throws ApiException {
    // create path and map variables
    String path = "/words.{format}/wordOfTheDay".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    if(!"null".equals(String.valueOf(date)))
      queryParams.put("date", String.valueOf(date));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (WordOfTheDay) ApiInvoker.deserialize(response, "", WordOfTheDay.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  public List<WordObject> getRandomWords (String hasDictionaryDef, String includePartOfSpeech, String excludePartOfSpeech, int minCorpusCount, int maxCorpusCount, int minDictionaryCount, int maxDictionaryCount, int minLength, int maxLength, String sortBy, String sortOrder, int limit) throws ApiException {
    // create path and map variables
    String path = "/words.{format}/randomWords".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    if(!"null".equals(String.valueOf(hasDictionaryDef)))
      queryParams.put("hasDictionaryDef", String.valueOf(hasDictionaryDef));
    if(!"null".equals(String.valueOf(includePartOfSpeech)))
      queryParams.put("includePartOfSpeech", String.valueOf(includePartOfSpeech));
    if(!"null".equals(String.valueOf(excludePartOfSpeech)))
      queryParams.put("excludePartOfSpeech", String.valueOf(excludePartOfSpeech));
    if(!"null".equals(String.valueOf(minCorpusCount)))
      queryParams.put("minCorpusCount", String.valueOf(minCorpusCount));
    if(!"null".equals(String.valueOf(maxCorpusCount)))
      queryParams.put("maxCorpusCount", String.valueOf(maxCorpusCount));
    if(!"null".equals(String.valueOf(minDictionaryCount)))
      queryParams.put("minDictionaryCount", String.valueOf(minDictionaryCount));
    if(!"null".equals(String.valueOf(maxDictionaryCount)))
      queryParams.put("maxDictionaryCount", String.valueOf(maxDictionaryCount));
    if(!"null".equals(String.valueOf(minLength)))
      queryParams.put("minLength", String.valueOf(minLength));
    if(!"null".equals(String.valueOf(maxLength)))
      queryParams.put("maxLength", String.valueOf(maxLength));
    if(!"null".equals(String.valueOf(sortBy)))
      queryParams.put("sortBy", String.valueOf(sortBy));
    if(!"null".equals(String.valueOf(sortOrder)))
      queryParams.put("sortOrder", String.valueOf(sortOrder));
    if(!"null".equals(String.valueOf(limit)))
      queryParams.put("limit", String.valueOf(limit));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (List<WordObject>) ApiInvoker.deserialize(response, "List", WordObject.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  public WordObject getRandomWord (String includePartOfSpeech, String excludePartOfSpeech, int minCorpusCount, int maxCorpusCount, int minDictionaryCount, int maxDictionaryCount, int minLength, int maxLength, String hasDictionaryDef) throws ApiException {
    // create path and map variables
    String path = "/words.{format}/randomWord".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    if(!"null".equals(String.valueOf(hasDictionaryDef)))
      queryParams.put("hasDictionaryDef", String.valueOf(hasDictionaryDef));
    if(!"null".equals(String.valueOf(includePartOfSpeech)))
      queryParams.put("includePartOfSpeech", String.valueOf(includePartOfSpeech));
    if(!"null".equals(String.valueOf(excludePartOfSpeech)))
      queryParams.put("excludePartOfSpeech", String.valueOf(excludePartOfSpeech));
    if(!"null".equals(String.valueOf(minCorpusCount)))
      queryParams.put("minCorpusCount", String.valueOf(minCorpusCount));
    if(!"null".equals(String.valueOf(maxCorpusCount)))
      queryParams.put("maxCorpusCount", String.valueOf(maxCorpusCount));
    if(!"null".equals(String.valueOf(minDictionaryCount)))
      queryParams.put("minDictionaryCount", String.valueOf(minDictionaryCount));
    if(!"null".equals(String.valueOf(maxDictionaryCount)))
      queryParams.put("maxDictionaryCount", String.valueOf(maxDictionaryCount));
    if(!"null".equals(String.valueOf(minLength)))
      queryParams.put("minLength", String.valueOf(minLength));
    if(!"null".equals(String.valueOf(maxLength)))
      queryParams.put("maxLength", String.valueOf(maxLength));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (WordObject) ApiInvoker.deserialize(response, "", WordObject.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  }
