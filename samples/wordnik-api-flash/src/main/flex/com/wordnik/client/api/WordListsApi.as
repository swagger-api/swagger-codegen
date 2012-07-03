package com.wordnik.client.api {

import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.exception.ApiErrorCodes;
import com.wordnik.swagger.exception.ApiError;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.event.Response;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.client.model.WordList;
import mx.rpc.AsyncToken;
import mx.utils.UIDUtil;
import flash.utils.Dictionary;
import flash.events.EventDispatcher;

public class WordListsApi extends SwaggerApi {
    /**
    * Constructor for the WordListsApi api client
    * @param apiCredentials Wrapper object for tokens and hostName required towards authentication
    * @param eventDispatcher Optional event dispatcher that when provided is used by the SDK to dispatch any Response
    */
    public function WordListsApi(apiCredentials: ApiUserCredentials, eventDispatcher: EventDispatcher = null) {
        super(apiCredentials, eventDispatcher);
    }

public static const event_createWordList: String = "createWordList";
/*
     * Returns WordList */
    public function createWordList (body: WordList, auth_token: String): String {
        // create path and map variables
        var path: String = "/wordLists.{format}".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(auth_token == null ) {
            throw new ApiError(400, "missing required params");
        }
        headerParams["auth_token"] = toPathValue(auth_token);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "createWordList";

        token.returnType = WordList;
        return requestId;

    }
    }
        }
