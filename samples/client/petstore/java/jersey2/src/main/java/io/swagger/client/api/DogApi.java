package io.swagger.client.api;

import io.swagger.client.ApiException;
import io.swagger.client.ApiClient;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;

import javax.ws.rs.core.GenericType;

import io.swagger.client.model.Dog;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DogApi {
  private ApiClient apiClient;

  public DogApi() {
    this(Configuration.getDefaultApiClient());
  }

  public DogApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  /**
   * Add a new dog to the store
   * 
   * @param body Dog object that needs to be added to the store (required)
   * @throws ApiException if fails to make API call
   */
  public void addDog(Dog body) throws ApiException {
    Object localVarPostBody = body;
    // verify the required parameter 'body' is set
    if (body == null) {
      throw new ApiException(400, "Missing the required parameter 'body' when calling addDog");
    }
    // create path and map variables
    String localVarPath = "/dog";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();




    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json", "application/xml"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * Deletes a dog
   * 
   * @param dogId Dog id to delete (required)
   * @param apiKey  (optional)
   * @throws ApiException if fails to make API call
   */
  public void deleteDog(Long dogId, String apiKey) throws ApiException {
    Object localVarPostBody = null;
    // verify the required parameter 'dogId' is set
    if (dogId == null) {
      throw new ApiException(400, "Missing the required parameter 'dogId' when calling deleteDog");
    }
    // create path and map variables
    String localVarPath = "/dog/{dogId}"
      .replaceAll("\\{" + "dogId" + "\\}", apiClient.escapeString(dogId.toString()));

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    if (apiKey != null)
      localVarHeaderParams.put("api_key", apiClient.parameterToString(apiKey));


    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    apiClient.invokeAPI(localVarPath, "DELETE", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * Find dog by ID
   * Returns a single dog
   * @param dogId ID of dog to return (required)
   * @return Dog
   * @throws ApiException if fails to make API call
   */
  public Dog getDogById(Long dogId) throws ApiException {
    Object localVarPostBody = null;
    // verify the required parameter 'dogId' is set
    if (dogId == null) {
      throw new ApiException(400, "Missing the required parameter 'dogId' when calling getDogById");
    }
    // create path and map variables
    String localVarPath = "/dog/{dogId}"
      .replaceAll("\\{" + "dogId" + "\\}", apiClient.escapeString(dogId.toString()));

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();




    final String[] localVarAccepts = {
      "application/xml", "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<Dog> localVarReturnType = new GenericType<Dog>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }
  /**
   * Update an existing dog
   * 
   * @param body Dog object that needs to be added. (required)
   * @throws ApiException if fails to make API call
   */
  public void updateDog(Dog body) throws ApiException {
    Object localVarPostBody = body;
    // verify the required parameter 'body' is set
    if (body == null) {
      throw new ApiException(400, "Missing the required parameter 'body' when calling updateDog");
    }
    // create path and map variables
    String localVarPath = "/dog";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();




    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json", "application/xml"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * Updates a dog
   * 
   * @param dogId ID of dog that needs to be updated (required)
   * @param name  (optional)
   * @param status  (optional)
   * @throws ApiException if fails to make API call
   */
  public void updateDogWithForm(Long dogId, String name, String status) throws ApiException {
    Object localVarPostBody = null;
    // verify the required parameter 'dogId' is set
    if (dogId == null) {
      throw new ApiException(400, "Missing the required parameter 'dogId' when calling updateDogWithForm");
    }
    // create path and map variables
    String localVarPath = "/dog/{dogId}"
      .replaceAll("\\{" + "dogId" + "\\}", apiClient.escapeString(dogId.toString()));

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();



    if (name != null)
      localVarFormParams.put("name", name);
    if (status != null)
      localVarFormParams.put("status", status);

    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/x-www-form-urlencoded"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
}
