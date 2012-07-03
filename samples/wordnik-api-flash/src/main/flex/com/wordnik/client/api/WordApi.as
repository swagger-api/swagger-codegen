package com.wordnik.client.api {

import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.exception.ApiErrorCodes;
import com.wordnik.swagger.exception.ApiError;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.event.Response;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.client.model.Definition;
import com.wordnik.client.model.TextPron;
import com.wordnik.client.model.Example;
import com.wordnik.client.model.Syllable;
import com.wordnik.client.model.AudioFile;
import com.wordnik.client.model.ExampleSearchResults;
import com.wordnik.client.model.WordObject;
import com.wordnik.client.model.Bigram;
import com.wordnik.client.model.Related;
import com.wordnik.client.model.FrequencySummary;
import mx.rpc.AsyncToken;
import mx.utils.UIDUtil;
import flash.utils.Dictionary;
import flash.events.EventDispatcher;

public class WordApi extends SwaggerApi {
    /**
    * Constructor for the WordApi api client
    * @param apiCredentials Wrapper object for tokens and hostName required towards authentication
    * @param eventDispatcher Optional event dispatcher that when provided is used by the SDK to dispatch any Response
    */
    public function WordApi(apiCredentials: ApiUserCredentials, eventDispatcher: EventDispatcher = null) {
        super(apiCredentials, eventDispatcher);
    }

public static const event_getExamples: String = "getExamples";
public static const event_getWord: String = "getWord";
public static const event_getDefinitions: String = "getDefinitions";
public static const event_getTopExample: String = "getTopExample";
public static const event_getRelatedWords: String = "getRelatedWords";
public static const event_getTextPronunciations: String = "getTextPronunciations";
public static const event_getHyphenation: String = "getHyphenation";
public static const event_getWordFrequency: String = "getWordFrequency";
public static const event_getPhrases: String = "getPhrases";
public static const event_getAudio: String = "getAudio";
/*
     * Returns ExampleSearchResults */
    public function getExamples (word: String, skip: Number, limit: Number, includeDuplicates: String= "false", useCanonical: String= "false"): String {
        // create path and map variables
        var path: String = "/word.{format}/{word}/examples".replace(/{format}/g,"xml").replace("{" + "word" + "}", getApiInvoker().escapeString(word));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(word == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(includeDuplicates))
            queryParams["includeDuplicates"] = toPathValue(includeDuplicates);
        if("null" != String(useCanonical))
            queryParams["useCanonical"] = toPathValue(useCanonical);
        if("null" != String(skip))
            queryParams["skip"] = toPathValue(skip);
        if("null" != String(limit))
            queryParams["limit"] = toPathValue(limit);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getExamples";

        token.returnType = ExampleSearchResults;
        return requestId;

    }
    /*
     * Returns WordObject */
    public function getWord (word: String, useCanonical: String= "false", includeSuggestions: String= "true"): String {
        // create path and map variables
        var path: String = "/word.{format}/{word}".replace(/{format}/g,"xml").replace("{" + "word" + "}", getApiInvoker().escapeString(word));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(word == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(useCanonical))
            queryParams["useCanonical"] = toPathValue(useCanonical);
        if("null" != String(includeSuggestions))
            queryParams["includeSuggestions"] = toPathValue(includeSuggestions);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getWord";

        token.returnType = WordObject;
        return requestId;

    }
    /*
     * Returns com.wordnik.client.model.DefinitionList */
    public function getDefinitions (word: String, limit: Number, partOfSpeech: String, sourceDictionaries: String, includeRelated: String= "false", useCanonical: String= "false", includeTags: String= "false"): String {
        // create path and map variables
        var path: String = "/word.{format}/{word}/definitions".replace(/{format}/g,"xml").replace("{" + "word" + "}", getApiInvoker().escapeString(word));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(word == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(limit))
            queryParams["limit"] = toPathValue(limit);
        if("null" != String(partOfSpeech))
            queryParams["partOfSpeech"] = toPathValue(partOfSpeech);
        if("null" != String(includeRelated))
            queryParams["includeRelated"] = toPathValue(includeRelated);
        if("null" != String(sourceDictionaries))
            queryParams["sourceDictionaries"] = toPathValue(sourceDictionaries);
        if("null" != String(useCanonical))
            queryParams["useCanonical"] = toPathValue(useCanonical);
        if("null" != String(includeTags))
            queryParams["includeTags"] = toPathValue(includeTags);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getDefinitions";

        token.returnType = com.wordnik.client.model.DefinitionList;
        return requestId;

    }
    /*
     * Returns Example */
    public function getTopExample (word: String, useCanonical: String= "false"): String {
        // create path and map variables
        var path: String = "/word.{format}/{word}/topExample".replace(/{format}/g,"xml").replace("{" + "word" + "}", getApiInvoker().escapeString(word));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(word == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(useCanonical))
            queryParams["useCanonical"] = toPathValue(useCanonical);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getTopExample";

        token.returnType = Example;
        return requestId;

    }
    /*
     * Returns com.wordnik.client.model.RelatedList */
    public function getRelatedWords (word: String, relationshipTypes: String, limitPerRelationshipType: Number, useCanonical: String= "false"): String {
        // create path and map variables
        var path: String = "/word.{format}/{word}/relatedWords".replace(/{format}/g,"xml").replace("{" + "word" + "}", getApiInvoker().escapeString(word));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(word == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(useCanonical))
            queryParams["useCanonical"] = toPathValue(useCanonical);
        if("null" != String(relationshipTypes))
            queryParams["relationshipTypes"] = toPathValue(relationshipTypes);
        if("null" != String(limitPerRelationshipType))
            queryParams["limitPerRelationshipType"] = toPathValue(limitPerRelationshipType);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getRelatedWords";

        token.returnType = com.wordnik.client.model.RelatedList;
        return requestId;

    }
    /*
     * Returns com.wordnik.client.model.TextPronList */
    public function getTextPronunciations (word: String, sourceDictionary: String, typeFormat: String, limit: Number, useCanonical: String= "false"): String {
        // create path and map variables
        var path: String = "/word.{format}/{word}/pronunciations".replace(/{format}/g,"xml").replace("{" + "word" + "}", getApiInvoker().escapeString(word));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(word == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(useCanonical))
            queryParams["useCanonical"] = toPathValue(useCanonical);
        if("null" != String(sourceDictionary))
            queryParams["sourceDictionary"] = toPathValue(sourceDictionary);
        if("null" != String(typeFormat))
            queryParams["typeFormat"] = toPathValue(typeFormat);
        if("null" != String(limit))
            queryParams["limit"] = toPathValue(limit);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getTextPronunciations";

        token.returnType = com.wordnik.client.model.TextPronList;
        return requestId;

    }
    /*
     * Returns com.wordnik.client.model.SyllableList */
    public function getHyphenation (word: String, sourceDictionary: String, limit: Number, useCanonical: String= "false"): String {
        // create path and map variables
        var path: String = "/word.{format}/{word}/hyphenation".replace(/{format}/g,"xml").replace("{" + "word" + "}", getApiInvoker().escapeString(word));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(word == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(useCanonical))
            queryParams["useCanonical"] = toPathValue(useCanonical);
        if("null" != String(sourceDictionary))
            queryParams["sourceDictionary"] = toPathValue(sourceDictionary);
        if("null" != String(limit))
            queryParams["limit"] = toPathValue(limit);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getHyphenation";

        token.returnType = com.wordnik.client.model.SyllableList;
        return requestId;

    }
    /*
     * Returns FrequencySummary */
    public function getWordFrequency (word: String, startYear: Number, endYear: Number, useCanonical: String= "false"): String {
        // create path and map variables
        var path: String = "/word.{format}/{word}/frequency".replace(/{format}/g,"xml").replace("{" + "word" + "}", getApiInvoker().escapeString(word));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(word == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(useCanonical))
            queryParams["useCanonical"] = toPathValue(useCanonical);
        if("null" != String(startYear))
            queryParams["startYear"] = toPathValue(startYear);
        if("null" != String(endYear))
            queryParams["endYear"] = toPathValue(endYear);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getWordFrequency";

        token.returnType = FrequencySummary;
        return requestId;

    }
    /*
     * Returns com.wordnik.client.model.BigramList */
    public function getPhrases (word: String, limit: Number, wlmi: Number, useCanonical: String= "false"): String {
        // create path and map variables
        var path: String = "/word.{format}/{word}/phrases".replace(/{format}/g,"xml").replace("{" + "word" + "}", getApiInvoker().escapeString(word));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(word == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(limit))
            queryParams["limit"] = toPathValue(limit);
        if("null" != String(wlmi))
            queryParams["wlmi"] = toPathValue(wlmi);
        if("null" != String(useCanonical))
            queryParams["useCanonical"] = toPathValue(useCanonical);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getPhrases";

        token.returnType = com.wordnik.client.model.BigramList;
        return requestId;

    }
    /*
     * Returns com.wordnik.client.model.AudioFileList */
    public function getAudio (word: String, limit: Number, useCanonical: String= "false"): String {
        // create path and map variables
        var path: String = "/word.{format}/{word}/audio".replace(/{format}/g,"xml").replace("{" + "word" + "}", getApiInvoker().escapeString(word));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(word == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(useCanonical))
            queryParams["useCanonical"] = toPathValue(useCanonical);
        if("null" != String(limit))
            queryParams["limit"] = toPathValue(limit);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getAudio";

        token.returnType = com.wordnik.client.model.AudioFileList;
        return requestId;

    }
    }
        }
