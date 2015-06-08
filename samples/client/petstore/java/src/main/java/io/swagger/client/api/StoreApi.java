package io.swagger.client.api;

import io.swagger.client.ApiException;
import io.swagger.client.ApiClient;
import io.swagger.client.Configuration;

import io.swagger.client.model.*;

import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;

import java.util.*;

import java.util.Map;
import io.swagger.client.model.Order;

import java.io.File;
import java.util.Map;
import java.util.HashMap;

public class StoreApi {
  private ApiClient apiClient;

  public StoreApi() {
    this(Configuration.getDefaultApiClient());
  }

  public StoreApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  
  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return Map<String, Integer>
   */
  public Map<String, Integer> getInventory() throws ApiException {
    Object postBody = null;
    

    // create path and map variables
    String path = "/store/inventory".replaceAll("\\{format\\}","json");

    Map<String, Object> queryParams = new HashMap<String, Object>();

    Map<String, Object> headerParams = new HashMap<String, Object>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] { "api_key" };
    Type returnType = new TypeToken<Map<String, Integer>>(){}.getType();
    return apiClient.invokeAPI(path, "GET", queryParams, postBody, headerParams, formParams, returnType, authNames);
  }
  
  /**
   * Place an order for a pet
   * 
   * @param body order placed for purchasing the pet
   * @return Order
   */
  public Order placeOrder(Order body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/store/order".replaceAll("\\{format\\}","json");

    Map<String, Object> queryParams = new HashMap<String, Object>();

    Map<String, Object> headerParams = new HashMap<String, Object>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    Type returnType = new TypeToken<Order>(){}.getType();
    return apiClient.invokeAPI(path, "POST", queryParams, postBody, headerParams, formParams, returnType, authNames);
  }
  
  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched
   * @return Order
   */
  public Order getOrderById(String orderId) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'orderId' is set
    if (orderId == null) {
       throw new ApiException(400, "Missing the required parameter 'orderId' when calling getOrderById");
    }
    

    // create path and map variables
    String path = "/store/order/{orderId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "orderId" + "\\}", apiClient.escapeString(orderId.toString()));

    Map<String, Object> queryParams = new HashMap<String, Object>();

    Map<String, Object> headerParams = new HashMap<String, Object>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    Type returnType = new TypeToken<Order>(){}.getType();
    return apiClient.invokeAPI(path, "GET", queryParams, postBody, headerParams, formParams, returnType, authNames);
  }
  
  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted
   * @return void
   */
  public void deleteOrder(String orderId) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'orderId' is set
    if (orderId == null) {
       throw new ApiException(400, "Missing the required parameter 'orderId' when calling deleteOrder");
    }
    

    // create path and map variables
    String path = "/store/order/{orderId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "orderId" + "\\}", apiClient.escapeString(orderId.toString()));

    Map<String, Object> queryParams = new HashMap<String, Object>();

    Map<String, Object> headerParams = new HashMap<String, Object>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    apiClient.invokeAPI(path, "DELETE", queryParams, postBody, headerParams, formParams, null, authNames);
  }
  
}
