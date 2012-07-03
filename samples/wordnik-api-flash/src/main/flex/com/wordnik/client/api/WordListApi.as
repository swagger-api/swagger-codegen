package com.wordnik.client.api {

import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.exception.ApiErrorCodes;
import com.wordnik.swagger.exception.ApiError;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.event.Response;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.client.model.WordList;
import com.wordnik.client.model.StringValue;
import com.wordnik.client.model.WordListWord;
import mx.rpc.AsyncToken;
import mx.utils.UIDUtil;
import flash.utils.Dictionary;
import flash.events.EventDispatcher;

public class WordListApi extends SwaggerApi {
    /**
    * Constructor for the WordListApi api client
    * @param apiCredentials Wrapper object for tokens and hostName required towards authentication
    * @param eventDispatcher Optional event dispatcher that when provided is used by the SDK to dispatch any Response
    */
    public function WordListApi(apiCredentials: ApiUserCredentials, eventDispatcher: EventDispatcher = null) {
        super(apiCredentials, eventDispatcher);
    }

public static const event_updateWordList: String = "updateWordList";
public static const event_deleteWordList: String = "deleteWordList";
public static const event_getWordListByPermalink: String = "getWordListByPermalink";
public static const event_addWordsToWordList: String = "addWordsToWordList";
public static const event_getWordListWords: String = "getWordListWords";
public static const event_deleteWordsFromWordList: String = "deleteWordsFromWordList";
/*
     * Returns void */
    public function updateWordList (permalink: String, body: WordList, auth_token: String): String {
        // create path and map variables
        var path: String = "/wordList.{format}/{permalink}".replace(/{format}/g,"xml").replace("{" + "permalink" + "}", getApiInvoker().escapeString(permalink));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(permalink == null || auth_token == null ) {
            throw new ApiError(400, "missing required params");
        }
        headerParams["auth_token"] = toPathValue(auth_token);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "PUT", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "updateWordList";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns void */
    public function deleteWordList (permalink: String, auth_token: String): String {
        // create path and map variables
        var path: String = "/wordList.{format}/{permalink}".replace(/{format}/g,"xml").replace("{" + "permalink" + "}", getApiInvoker().escapeString(permalink));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(permalink == null || auth_token == null ) {
            throw new ApiError(400, "missing required params");
        }
        headerParams["auth_token"] = toPathValue(auth_token);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "DELETE", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "deleteWordList";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns WordList */
    public function getWordListByPermalink (permalink: String, auth_token: String): String {
        // create path and map variables
        var path: String = "/wordList.{format}/{permalink}".replace(/{format}/g,"xml").replace("{" + "permalink" + "}", getApiInvoker().escapeString(permalink));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(permalink == null || auth_token == null ) {
            throw new ApiError(400, "missing required params");
        }
        headerParams["auth_token"] = toPathValue(auth_token);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getWordListByPermalink";

        token.returnType = WordList;
        return requestId;

    }
    /*
     * Returns void */
    public function addWordsToWordList (permalink: String, body: Array, auth_token: String): String {
        // create path and map variables
        var path: String = "/wordList.{format}/{permalink}/words".replace(/{format}/g,"xml").replace("{" + "permalink" + "}", getApiInvoker().escapeString(permalink));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(permalink == null || auth_token == null ) {
            throw new ApiError(400, "missing required params");
        }
        headerParams["auth_token"] = toPathValue(auth_token);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "addWordsToWordList";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns com.wordnik.client.model.WordListWordList */
    public function getWordListWords (permalink: String, skip: Number, limit: Number, auth_token: String, sortBy: String= "createDate", sortOrder: String= "desc"): String {
        // create path and map variables
        var path: String = "/wordList.{format}/{permalink}/words".replace(/{format}/g,"xml").replace("{" + "permalink" + "}", getApiInvoker().escapeString(permalink));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(permalink == null || auth_token == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(sortBy))
            queryParams["sortBy"] = toPathValue(sortBy);
        if("null" != String(sortOrder))
            queryParams["sortOrder"] = toPathValue(sortOrder);
        if("null" != String(skip))
            queryParams["skip"] = toPathValue(skip);
        if("null" != String(limit))
            queryParams["limit"] = toPathValue(limit);
        headerParams["auth_token"] = toPathValue(auth_token);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getWordListWords";

        token.returnType = com.wordnik.client.model.WordListWordList;
        return requestId;

    }
    /*
     * Returns void */
    public function deleteWordsFromWordList (permalink: String, body: Array, auth_token: String): String {
        // create path and map variables
        var path: String = "/wordList.{format}/{permalink}/deleteWords".replace(/{format}/g,"xml").replace("{" + "permalink" + "}", getApiInvoker().escapeString(permalink));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(permalink == null || auth_token == null ) {
            throw new ApiError(400, "missing required params");
        }
        headerParams["auth_token"] = toPathValue(auth_token);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "deleteWordsFromWordList";

        token.returnType = null ;
        return requestId;

    }
    }
        }
