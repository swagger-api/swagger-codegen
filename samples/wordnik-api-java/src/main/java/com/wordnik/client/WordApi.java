package com.wordnik.client;

import com.wordnik.client.ApiException;
import com.wordnik.client.ApiInvoker;
import com.wordnik.client.model.Frequency;
import com.wordnik.client.model.Facet;
import java.util.Date;
import com.wordnik.client.model.Definition;
import com.wordnik.client.model.TextPron;
import com.wordnik.client.model.AudioFile;
import com.wordnik.client.model.Example;
import com.wordnik.client.model.Syllable;
import com.wordnik.client.model.PartOfSpeech;
import com.wordnik.client.model.ExampleSearchResults;
import com.wordnik.client.model.WordObject;
import com.wordnik.client.model.Bigram;
import com.wordnik.client.model.FrequencySummary;
import com.wordnik.client.model.Related;
import java.util.*;

public class WordApi {
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

  public ExampleSearchResults getExamples (String word, String includeDuplicates, String contentProvider, String useCanonical, String skip, String limit) throws ApiException {
    // create path and map variables
    String path = "/word.{format}/{word}/examples".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(word == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(includeDuplicates)))
      queryParams.put("includeDuplicates", String.valueOf(includeDuplicates));
    if(!"null".equals(String.valueOf(contentProvider)))
      queryParams.put("contentProvider", String.valueOf(contentProvider));
    if(!"null".equals(String.valueOf(useCanonical)))
      queryParams.put("useCanonical", String.valueOf(useCanonical));
    if(!"null".equals(String.valueOf(skip)))
      queryParams.put("skip", String.valueOf(skip));
    if(!"null".equals(String.valueOf(limit)))
      queryParams.put("limit", String.valueOf(limit));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (ExampleSearchResults) ApiInvoker.deserialize(response, "", ExampleSearchResults.class);
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
  public WordObject getWord (String word, String includeSuggestions, String useCanonical) throws ApiException {
    // create path and map variables
    String path = "/word.{format}/{word}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(word == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(useCanonical)))
      queryParams.put("useCanonical", String.valueOf(useCanonical));
    if(!"null".equals(String.valueOf(includeSuggestions)))
      queryParams.put("includeSuggestions", String.valueOf(includeSuggestions));
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
  public List<Definition> getDefinitions (String word, String limit, String partOfSpeech, String sourceDictionaries, String includeRelated, String useCanonical, String includeTags) throws ApiException {
    // create path and map variables
    String path = "/word.{format}/{word}/definitions".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(word == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(limit)))
      queryParams.put("limit", String.valueOf(limit));
    if(!"null".equals(String.valueOf(partOfSpeech)))
      queryParams.put("partOfSpeech", String.valueOf(partOfSpeech));
    if(!"null".equals(String.valueOf(includeRelated)))
      queryParams.put("includeRelated", String.valueOf(includeRelated));
    if(!"null".equals(String.valueOf(sourceDictionaries)))
      queryParams.put("sourceDictionaries", String.valueOf(sourceDictionaries));
    if(!"null".equals(String.valueOf(useCanonical)))
      queryParams.put("useCanonical", String.valueOf(useCanonical));
    if(!"null".equals(String.valueOf(includeTags)))
      queryParams.put("includeTags", String.valueOf(includeTags));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (List<Definition>) ApiInvoker.deserialize(response, "List", Definition.class);
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
  public Example getTopExample (String word, String contentProvider, String useCanonical) throws ApiException {
    // create path and map variables
    String path = "/word.{format}/{word}/topExample".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(word == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(contentProvider)))
      queryParams.put("contentProvider", String.valueOf(contentProvider));
    if(!"null".equals(String.valueOf(useCanonical)))
      queryParams.put("useCanonical", String.valueOf(useCanonical));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (Example) ApiInvoker.deserialize(response, "", Example.class);
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
  public List<TextPron> getTextPronunciations (String word, String useCanonical, String sourceDictionary, String typeFormat, String limit) throws ApiException {
    // create path and map variables
    String path = "/word.{format}/{word}/pronunciations".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(word == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(useCanonical)))
      queryParams.put("useCanonical", String.valueOf(useCanonical));
    if(!"null".equals(String.valueOf(sourceDictionary)))
      queryParams.put("sourceDictionary", String.valueOf(sourceDictionary));
    if(!"null".equals(String.valueOf(typeFormat)))
      queryParams.put("typeFormat", String.valueOf(typeFormat));
    if(!"null".equals(String.valueOf(limit)))
      queryParams.put("limit", String.valueOf(limit));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (List<TextPron>) ApiInvoker.deserialize(response, "List", TextPron.class);
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
  public List<Syllable> getHyphenation (String word, String useCanonical, String sourceDictionary, String limit) throws ApiException {
    // create path and map variables
    String path = "/word.{format}/{word}/hyphenation".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(word == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(useCanonical)))
      queryParams.put("useCanonical", String.valueOf(useCanonical));
    if(!"null".equals(String.valueOf(sourceDictionary)))
      queryParams.put("sourceDictionary", String.valueOf(sourceDictionary));
    if(!"null".equals(String.valueOf(limit)))
      queryParams.put("limit", String.valueOf(limit));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (List<Syllable>) ApiInvoker.deserialize(response, "List", Syllable.class);
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
  public FrequencySummary getWordFrequency (String word, String useCanonical, String startYear, String endYear) throws ApiException {
    // create path and map variables
    String path = "/word.{format}/{word}/frequency".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(word == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(useCanonical)))
      queryParams.put("useCanonical", String.valueOf(useCanonical));
    if(!"null".equals(String.valueOf(startYear)))
      queryParams.put("startYear", String.valueOf(startYear));
    if(!"null".equals(String.valueOf(endYear)))
      queryParams.put("endYear", String.valueOf(endYear));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (FrequencySummary) ApiInvoker.deserialize(response, "", FrequencySummary.class);
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
  public List<Bigram> getPhrases (String word, String limit, String wlmi, String useCanonical) throws ApiException {
    // create path and map variables
    String path = "/word.{format}/{word}/phrases".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(word == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(limit)))
      queryParams.put("limit", String.valueOf(limit));
    if(!"null".equals(String.valueOf(wlmi)))
      queryParams.put("wlmi", String.valueOf(wlmi));
    if(!"null".equals(String.valueOf(useCanonical)))
      queryParams.put("useCanonical", String.valueOf(useCanonical));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (List<Bigram>) ApiInvoker.deserialize(response, "List", Bigram.class);
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
  public List<Related> getRelated (String word, String partOfSpeech, String sourceDictionary, String limit, String useCanonical, String type) throws ApiException {
    // create path and map variables
    String path = "/word.{format}/{word}/related".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(word == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(partOfSpeech)))
      queryParams.put("partOfSpeech", String.valueOf(partOfSpeech));
    if(!"null".equals(String.valueOf(sourceDictionary)))
      queryParams.put("sourceDictionary", String.valueOf(sourceDictionary));
    if(!"null".equals(String.valueOf(limit)))
      queryParams.put("limit", String.valueOf(limit));
    if(!"null".equals(String.valueOf(useCanonical)))
      queryParams.put("useCanonical", String.valueOf(useCanonical));
    if(!"null".equals(String.valueOf(type)))
      queryParams.put("type", String.valueOf(type));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (List<Related>) ApiInvoker.deserialize(response, "List", Related.class);
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
  public List<AudioFile> getAudio (String word, String useCanonical, String limit) throws ApiException {
    // create path and map variables
    String path = "/word.{format}/{word}/audio".replaceAll("\\{format\\}","json").replaceAll("\\{" + "word" + "\\}", apiInvoker.escapeString(word));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(word == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(useCanonical)))
      queryParams.put("useCanonical", String.valueOf(useCanonical));
    if(!"null".equals(String.valueOf(limit)))
      queryParams.put("limit", String.valueOf(limit));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (List<AudioFile>) ApiInvoker.deserialize(response, "List", AudioFile.class);
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
