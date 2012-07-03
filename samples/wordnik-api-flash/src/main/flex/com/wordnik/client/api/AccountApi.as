package com.wordnik.client.api {

import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.exception.ApiErrorCodes;
import com.wordnik.swagger.exception.ApiError;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.event.Response;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.client.model.ApiTokenStatus;
import com.wordnik.client.model.WordList;
import com.wordnik.client.model.User;
import com.wordnik.client.model.AuthenticationToken;
import mx.rpc.AsyncToken;
import mx.utils.UIDUtil;
import flash.utils.Dictionary;
import flash.events.EventDispatcher;

public class AccountApi extends SwaggerApi {
    /**
    * Constructor for the AccountApi api client
    * @param apiCredentials Wrapper object for tokens and hostName required towards authentication
    * @param eventDispatcher Optional event dispatcher that when provided is used by the SDK to dispatch any Response
    */
    public function AccountApi(apiCredentials: ApiUserCredentials, eventDispatcher: EventDispatcher = null) {
        super(apiCredentials, eventDispatcher);
    }

public static const event_authenticate: String = "authenticate";
public static const event_authenticatePost: String = "authenticatePost";
public static const event_getWordListsForLoggedInUser: String = "getWordListsForLoggedInUser";
public static const event_getApiTokenStatus: String = "getApiTokenStatus";
public static const event_getLoggedInUser: String = "getLoggedInUser";
/*
     * Returns AuthenticationToken */
    public function authenticate (username: String, password: String): String {
        // create path and map variables
        var path: String = "/account.{format}/authenticate/{username}".replace(/{format}/g,"xml").replace("{" + "username" + "}", getApiInvoker().escapeString(username));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(username == null || password == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(password))
            queryParams["password"] = toPathValue(password);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "authenticate";

        token.returnType = AuthenticationToken;
        return requestId;

    }
    /*
     * Returns AuthenticationToken */
    public function authenticatePost (username: String, body: String): String {
        // create path and map variables
        var path: String = "/account.{format}/authenticate/{username}".replace(/{format}/g,"xml").replace("{" + "username" + "}", getApiInvoker().escapeString(username));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(username == null || body == null ) {
            throw new ApiError(400, "missing required params");
        }
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
            token.completionEventType = "authenticatePost";

        token.returnType = AuthenticationToken;
        return requestId;

    }
    /*
     * Returns com.wordnik.client.model.WordListList */
    public function getWordListsForLoggedInUser (auth_token: String, skip: Number, limit: Number): String {
        // create path and map variables
        var path: String = "/account.{format}/wordLists".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(auth_token == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(skip))
            queryParams["skip"] = toPathValue(skip);
        if("null" != String(limit))
            queryParams["limit"] = toPathValue(limit);
        headerParams["auth_token"] = toPathValue(auth_token);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getWordListsForLoggedInUser";

        token.returnType = com.wordnik.client.model.WordListList;
        return requestId;

    }
    /*
     * Returns ApiTokenStatus */
    public function getApiTokenStatus (api_key: String): String {
        // create path and map variables
        var path: String = "/account.{format}/apiTokenStatus".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        headerParams["api_key"] = toPathValue(api_key);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getApiTokenStatus";

        token.returnType = ApiTokenStatus;
        return requestId;

    }
    /*
     * Returns User */
    public function getLoggedInUser (auth_token: String): String {
        // create path and map variables
        var path: String = "/account.{format}/user".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(auth_token == null ) {
            throw new ApiError(400, "missing required params");
        }
        headerParams["auth_token"] = toPathValue(auth_token);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getLoggedInUser";

        token.returnType = User;
        return requestId;

    }
    }
        }
