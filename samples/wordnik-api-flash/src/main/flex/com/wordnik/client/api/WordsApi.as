package com.wordnik.client.api {

import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.exception.ApiErrorCodes;
import com.wordnik.swagger.exception.ApiError;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.event.Response;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.client.model.WordObject;
import com.wordnik.client.model.DefinitionSearchResults;
import com.wordnik.client.model.WordOfTheDay;
import com.wordnik.client.model.WordSearchResults;
import mx.rpc.AsyncToken;
import mx.utils.UIDUtil;
import flash.utils.Dictionary;
import flash.events.EventDispatcher;

public class WordsApi extends SwaggerApi {
    /**
    * Constructor for the WordsApi api client
    * @param apiCredentials Wrapper object for tokens and hostName required towards authentication
    * @param eventDispatcher Optional event dispatcher that when provided is used by the SDK to dispatch any Response
    */
    public function WordsApi(apiCredentials: ApiUserCredentials, eventDispatcher: EventDispatcher = null) {
        super(apiCredentials, eventDispatcher);
    }

public static const event_searchWords: String = "searchWords";
public static const event_getWordOfTheDay: String = "getWordOfTheDay";
public static const event_reverseDictionary: String = "reverseDictionary";
public static const event_getRandomWords: String = "getRandomWords";
public static const event_getRandomWord: String = "getRandomWord";
/*
     * Returns WordSearchResults */
    public function searchWords (query: String, includePartOfSpeech: String, excludePartOfSpeech: String, minCorpusCount: Number, maxCorpusCount: Number, minDictionaryCount: Number, maxDictionaryCount: Number, minLength: Number, maxLength: Number, skip: Number, limit: Number, caseSensitive: String= "true"): String {
        // create path and map variables
        var path: String = "/words.{format}/search/{query}".replace(/{format}/g,"xml").replace("{" + "query" + "}", getApiInvoker().escapeString(query));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(query == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(caseSensitive))
            queryParams["caseSensitive"] = toPathValue(caseSensitive);
        if("null" != String(includePartOfSpeech))
            queryParams["includePartOfSpeech"] = toPathValue(includePartOfSpeech);
        if("null" != String(excludePartOfSpeech))
            queryParams["excludePartOfSpeech"] = toPathValue(excludePartOfSpeech);
        if("null" != String(minCorpusCount))
            queryParams["minCorpusCount"] = toPathValue(minCorpusCount);
        if("null" != String(maxCorpusCount))
            queryParams["maxCorpusCount"] = toPathValue(maxCorpusCount);
        if("null" != String(minDictionaryCount))
            queryParams["minDictionaryCount"] = toPathValue(minDictionaryCount);
        if("null" != String(maxDictionaryCount))
            queryParams["maxDictionaryCount"] = toPathValue(maxDictionaryCount);
        if("null" != String(minLength))
            queryParams["minLength"] = toPathValue(minLength);
        if("null" != String(maxLength))
            queryParams["maxLength"] = toPathValue(maxLength);
        if("null" != String(skip))
            queryParams["skip"] = toPathValue(skip);
        if("null" != String(limit))
            queryParams["limit"] = toPathValue(limit);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "searchWords";

        token.returnType = WordSearchResults;
        return requestId;

    }
    /*
     * Returns WordOfTheDay */
    public function getWordOfTheDay (date: String): String {
        // create path and map variables
        var path: String = "/words.{format}/wordOfTheDay".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        if("null" != String(date))
            queryParams["date"] = toPathValue(date);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getWordOfTheDay";

        token.returnType = WordOfTheDay;
        return requestId;

    }
    /*
     * Returns DefinitionSearchResults */
    public function reverseDictionary (query: String, findSenseForWord: String, includeSourceDictionaries: String, excludeSourceDictionaries: String, includePartOfSpeech: String, excludePartOfSpeech: String, minCorpusCount: Number, maxCorpusCount: Number, minLength: Number, maxLength: Number, expandTerms: String, sortBy: String, sortOrder: String, limit: Number, includeTags: String= "false", skip: String= "0"): String {
        // create path and map variables
        var path: String = "/words.{format}/reverseDictionary".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(query == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(query))
            queryParams["query"] = toPathValue(query);
        if("null" != String(findSenseForWord))
            queryParams["findSenseForWord"] = toPathValue(findSenseForWord);
        if("null" != String(includeSourceDictionaries))
            queryParams["includeSourceDictionaries"] = toPathValue(includeSourceDictionaries);
        if("null" != String(excludeSourceDictionaries))
            queryParams["excludeSourceDictionaries"] = toPathValue(excludeSourceDictionaries);
        if("null" != String(includePartOfSpeech))
            queryParams["includePartOfSpeech"] = toPathValue(includePartOfSpeech);
        if("null" != String(excludePartOfSpeech))
            queryParams["excludePartOfSpeech"] = toPathValue(excludePartOfSpeech);
        if("null" != String(minCorpusCount))
            queryParams["minCorpusCount"] = toPathValue(minCorpusCount);
        if("null" != String(maxCorpusCount))
            queryParams["maxCorpusCount"] = toPathValue(maxCorpusCount);
        if("null" != String(minLength))
            queryParams["minLength"] = toPathValue(minLength);
        if("null" != String(maxLength))
            queryParams["maxLength"] = toPathValue(maxLength);
        if("null" != String(expandTerms))
            queryParams["expandTerms"] = toPathValue(expandTerms);
        if("null" != String(includeTags))
            queryParams["includeTags"] = toPathValue(includeTags);
        if("null" != String(sortBy))
            queryParams["sortBy"] = toPathValue(sortBy);
        if("null" != String(sortOrder))
            queryParams["sortOrder"] = toPathValue(sortOrder);
        if("null" != String(skip))
            queryParams["skip"] = toPathValue(skip);
        if("null" != String(limit))
            queryParams["limit"] = toPathValue(limit);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "reverseDictionary";

        token.returnType = DefinitionSearchResults;
        return requestId;

    }
    /*
     * Returns com.wordnik.client.model.WordObjectList */
    public function getRandomWords (includePartOfSpeech: String, excludePartOfSpeech: String, minCorpusCount: Number, maxCorpusCount: Number, minDictionaryCount: Number, maxDictionaryCount: Number, minLength: Number, maxLength: Number, sortBy: String, sortOrder: String, limit: Number, hasDictionaryDef: String= "true"): String {
        // create path and map variables
        var path: String = "/words.{format}/randomWords".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        if("null" != String(hasDictionaryDef))
            queryParams["hasDictionaryDef"] = toPathValue(hasDictionaryDef);
        if("null" != String(includePartOfSpeech))
            queryParams["includePartOfSpeech"] = toPathValue(includePartOfSpeech);
        if("null" != String(excludePartOfSpeech))
            queryParams["excludePartOfSpeech"] = toPathValue(excludePartOfSpeech);
        if("null" != String(minCorpusCount))
            queryParams["minCorpusCount"] = toPathValue(minCorpusCount);
        if("null" != String(maxCorpusCount))
            queryParams["maxCorpusCount"] = toPathValue(maxCorpusCount);
        if("null" != String(minDictionaryCount))
            queryParams["minDictionaryCount"] = toPathValue(minDictionaryCount);
        if("null" != String(maxDictionaryCount))
            queryParams["maxDictionaryCount"] = toPathValue(maxDictionaryCount);
        if("null" != String(minLength))
            queryParams["minLength"] = toPathValue(minLength);
        if("null" != String(maxLength))
            queryParams["maxLength"] = toPathValue(maxLength);
        if("null" != String(sortBy))
            queryParams["sortBy"] = toPathValue(sortBy);
        if("null" != String(sortOrder))
            queryParams["sortOrder"] = toPathValue(sortOrder);
        if("null" != String(limit))
            queryParams["limit"] = toPathValue(limit);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getRandomWords";

        token.returnType = com.wordnik.client.model.WordObjectList;
        return requestId;

    }
    /*
     * Returns WordObject */
    public function getRandomWord (includePartOfSpeech: String, excludePartOfSpeech: String, minCorpusCount: Number, maxCorpusCount: Number, minDictionaryCount: Number, maxDictionaryCount: Number, minLength: Number, maxLength: Number, hasDictionaryDef: String= "true"): String {
        // create path and map variables
        var path: String = "/words.{format}/randomWord".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        if("null" != String(hasDictionaryDef))
            queryParams["hasDictionaryDef"] = toPathValue(hasDictionaryDef);
        if("null" != String(includePartOfSpeech))
            queryParams["includePartOfSpeech"] = toPathValue(includePartOfSpeech);
        if("null" != String(excludePartOfSpeech))
            queryParams["excludePartOfSpeech"] = toPathValue(excludePartOfSpeech);
        if("null" != String(minCorpusCount))
            queryParams["minCorpusCount"] = toPathValue(minCorpusCount);
        if("null" != String(maxCorpusCount))
            queryParams["maxCorpusCount"] = toPathValue(maxCorpusCount);
        if("null" != String(minDictionaryCount))
            queryParams["minDictionaryCount"] = toPathValue(minDictionaryCount);
        if("null" != String(maxDictionaryCount))
            queryParams["maxDictionaryCount"] = toPathValue(maxDictionaryCount);
        if("null" != String(minLength))
            queryParams["minLength"] = toPathValue(minLength);
        if("null" != String(maxLength))
            queryParams["maxLength"] = toPathValue(maxLength);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getRandomWord";

        token.returnType = WordObject;
        return requestId;

    }
    }
        }
