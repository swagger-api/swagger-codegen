package io.swagger.client.api;

import java.math.BigDecimal;
import io.swagger.client.model.Client;
import io.swagger.client.model.OuterComposite;


import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.MultiMap;
import io.vertx.core.json.JsonObject;

import com.fasterxml.jackson.core.type.TypeReference;

import java.util.*;

import io.swagger.client.ApiClient;
import io.swagger.client.ApiException;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;



public class FakeApiImpl implements FakeApi {

    private ApiClient apiClient;

    public FakeApiImpl() {
        this(null);
    }

    public FakeApiImpl(ApiClient apiClient) {
        this.apiClient = apiClient != null ? apiClient : Configuration.getDefaultApiClient();
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    
    /**
     * 
     * Test serialization of outer boolean types
     
     * @param body Input boolean as post body (optional)
     
     * @param resultHandler Asynchronous result handler
     */
    public void fakeOuterBooleanSerialize(Boolean body, Handler<AsyncResult<Boolean>> resultHandler) {
        Object localVarBody = body;
        
        // create path and map variables
        String localVarPath = "/fake/outer/boolean";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        

        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        

        String[] localVarAccepts = { "*/*" };
        String[] localVarContentTypes = { "*/*" };
        String[] localVarAuthNames = new String[] {  };
        
        TypeReference<Boolean> localVarReturnType = new TypeReference<Boolean>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, localVarReturnType, resultHandler);
    }
    
    /**
     * 
     * Test serialization of object with outer number type
     
     * @param outercomposite Input composite as post body (optional)
     
     * @param resultHandler Asynchronous result handler
     */
    public void fakeOuterCompositeSerialize(OuterComposite outercomposite, Handler<AsyncResult<OuterComposite>> resultHandler) {
        Object localVarBody = outercomposite;
        
        // create path and map variables
        String localVarPath = "/fake/outer/composite";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        

        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        

        String[] localVarAccepts = { "*/*" };
        String[] localVarContentTypes = { "*/*" };
        String[] localVarAuthNames = new String[] {  };
        
        TypeReference<OuterComposite> localVarReturnType = new TypeReference<OuterComposite>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, localVarReturnType, resultHandler);
    }
    
    /**
     * 
     * Test serialization of outer number types
     
     * @param body Input number as post body (optional)
     
     * @param resultHandler Asynchronous result handler
     */
    public void fakeOuterNumberSerialize(BigDecimal body, Handler<AsyncResult<BigDecimal>> resultHandler) {
        Object localVarBody = body;
        
        // create path and map variables
        String localVarPath = "/fake/outer/number";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        

        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        

        String[] localVarAccepts = { "*/*" };
        String[] localVarContentTypes = { "*/*" };
        String[] localVarAuthNames = new String[] {  };
        
        TypeReference<BigDecimal> localVarReturnType = new TypeReference<BigDecimal>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, localVarReturnType, resultHandler);
    }
    
    /**
     * 
     * Test serialization of outer string types
     
     * @param body Input string as post body (optional)
     
     * @param resultHandler Asynchronous result handler
     */
    public void fakeOuterStringSerialize(String body, Handler<AsyncResult<String>> resultHandler) {
        Object localVarBody = body;
        
        // create path and map variables
        String localVarPath = "/fake/outer/string";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        

        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        

        String[] localVarAccepts = { "*/*" };
        String[] localVarContentTypes = { "*/*" };
        String[] localVarAuthNames = new String[] {  };
        
        TypeReference<String> localVarReturnType = new TypeReference<String>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, localVarReturnType, resultHandler);
    }
    
    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     
     * @param client client model (required)
     
     * @param resultHandler Asynchronous result handler
     */
    public void testClientModel(Client client, Handler<AsyncResult<Client>> resultHandler) {
        Object localVarBody = client;
        
        // verify the required parameter 'client' is set
        if (client == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'client' when calling testClientModel"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        

        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        

        String[] localVarAccepts = { "application/json" };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };
        
        TypeReference<Client> localVarReturnType = new TypeReference<Client>() {};
        apiClient.invokeAPI(localVarPath, "PATCH", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, localVarReturnType, resultHandler);
    }
    
    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     
     * @param body  (required)
     
     * @param resultHandler Asynchronous result handler
     */
    public void testEndpointParameters(Object body, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = body;
        
        // verify the required parameter 'body' is set
        if (body == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'body' when calling testEndpointParameters"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        

        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        

        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "application/xml; charset=utf-8", "application/json; charset=utf-8" };
        String[] localVarAuthNames = new String[] { "http_basic_test" };
        
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, null, resultHandler);
    }
    
    /**
     * To test enum parameters
     * To test enum parameters
     
     * @param body  (optional)
     
     * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
     
     * @param enumHeaderString Header parameter enum test (string) (optional)
     
     * @param enumQueryStringArray Query parameter enum test (string array) (optional)
     
     * @param enumQueryString Query parameter enum test (string) (optional)
     
     * @param enumQueryInteger Query parameter enum test (double) (optional)
     
     * @param resultHandler Asynchronous result handler
     */
    public void testEnumParameters(Object body, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = body;
        
        // create path and map variables
        String localVarPath = "/fake";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_string_array", enumQueryStringArray));
        
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_string", enumQueryString));
        
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_integer", enumQueryInteger));
        

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        if (enumHeaderStringArray != null)
        localVarHeaderParams.add("enum_header_string_array", apiClient.parameterToString(enumHeaderStringArray));
        if (enumHeaderString != null)
        localVarHeaderParams.add("enum_header_string", apiClient.parameterToString(enumHeaderString));
        

        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        

        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "*/*" };
        String[] localVarAuthNames = new String[] {  };
        
        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, null, resultHandler);
    }
    
    /**
     * test inline additionalProperties
     * 
     
     * @param body request body (required)
     
     * @param resultHandler Asynchronous result handler
     */
    public void testInlineAdditionalProperties(Map<String, String> body, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = body;
        
        // verify the required parameter 'body' is set
        if (body == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'body' when calling testInlineAdditionalProperties"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake/inline-additionalProperties";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        

        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        

        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };
        
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, null, resultHandler);
    }
    
    /**
     * test json serialization of form data
     * 
     
     * @param body  (required)
     
     * @param resultHandler Asynchronous result handler
     */
    public void testJsonFormData(Object body, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = body;
        
        // verify the required parameter 'body' is set
        if (body == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'body' when calling testJsonFormData"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake/jsonFormData";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        

        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        

        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };
        
        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, null, resultHandler);
    }
    
}

