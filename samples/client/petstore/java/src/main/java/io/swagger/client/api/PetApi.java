package io.swagger.client.api;

import io.swagger.client.ApiException;
import io.swagger.client.ApiClient;
import io.swagger.client.Configuration;

import io.swagger.client.model.*;

import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;

import java.util.*;

import io.swagger.client.model.Pet;
import java.io.File;

import java.io.File;
import java.util.Map;
import java.util.HashMap;

public class PetApi {
  private ApiClient apiClient;

  public PetApi() {
    this(Configuration.getDefaultApiClient());
  }

  public PetApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  
  /**
   * Update an existing pet
   * 
   * @param body Pet object that needs to be added to the store
   * @return void
   */
  public void updatePet(Pet body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/pet".replaceAll("\\{format\\}","json");

    Map<String, Object> queryParams = new HashMap<String, Object>();

    Map<String, Object> headerParams = new HashMap<String, Object>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    String[] contentTypes = {
      "application/json","application/xml"
    };

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    apiClient.invokeAPI(path, "PUT", queryParams, postBody, headerParams, formParams, contentType, null);
  }
  
  /**
   * Add a new pet to the store
   * 
   * @param body Pet object that needs to be added to the store
   * @return void
   */
  public void addPet(Pet body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/pet".replaceAll("\\{format\\}","json");

    Map<String, Object> queryParams = new HashMap<String, Object>();

    Map<String, Object> headerParams = new HashMap<String, Object>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    String[] contentTypes = {
      "application/json","application/xml"
    };

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    apiClient.invokeAPI(path, "POST", queryParams, postBody, headerParams, formParams, contentType, null);
  }
  
  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma seperated strings
   * @param status Status values that need to be considered for filter
   * @return List<Pet>
   */
  public List<Pet> findPetsByStatus(List<String> status) throws ApiException {
    Object postBody = null;
    

    // create path and map variables
    String path = "/pet/findByStatus".replaceAll("\\{format\\}","json");

    Map<String, Object> queryParams = new HashMap<String, Object>();
    if (status != null)
      queryParams.put("status", status);

    Map<String, Object> headerParams = new HashMap<String, Object>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    String[] contentTypes = {
      
    };

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    Type returnType = new TypeToken<List<Pet>>(){}.getType();
    return apiClient.invokeAPI(path, "GET", queryParams, postBody, headerParams, formParams, contentType, returnType);
  }
  
  /**
   * Finds Pets by tags
   * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by
   * @return List<Pet>
   */
  public List<Pet> findPetsByTags(List<String> tags) throws ApiException {
    Object postBody = null;
    

    // create path and map variables
    String path = "/pet/findByTags".replaceAll("\\{format\\}","json");

    Map<String, Object> queryParams = new HashMap<String, Object>();
    if (tags != null)
      queryParams.put("tags", tags);

    Map<String, Object> headerParams = new HashMap<String, Object>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    String[] contentTypes = {
      
    };

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    Type returnType = new TypeToken<List<Pet>>(){}.getType();
    return apiClient.invokeAPI(path, "GET", queryParams, postBody, headerParams, formParams, contentType, returnType);
  }
  
  /**
   * Find pet by ID
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched
   * @return Pet
   */
  public Pet getPetById(Long petId) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
       throw new ApiException(400, "Missing the required parameter 'petId' when calling getPetById");
    }
    

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    Map<String, Object> queryParams = new HashMap<String, Object>();

    Map<String, Object> headerParams = new HashMap<String, Object>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    String[] contentTypes = {
      
    };

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    Type returnType = new TypeToken<Pet>(){}.getType();
    return apiClient.invokeAPI(path, "GET", queryParams, postBody, headerParams, formParams, contentType, returnType);
  }
  
  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated
   * @param name Updated name of the pet
   * @param status Updated status of the pet
   * @return void
   */
  public void updatePetWithForm(String petId, String name, String status) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
       throw new ApiException(400, "Missing the required parameter 'petId' when calling updatePetWithForm");
    }
    

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    Map<String, Object> queryParams = new HashMap<String, Object>();

    Map<String, Object> headerParams = new HashMap<String, Object>();

    Map<String, Object> formParams = new HashMap<String, Object>();
    formParams.put("name", name);
    formParams.put("status", status);

    String[] contentTypes = {
      "application/x-www-form-urlencoded"
    };

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    apiClient.invokeAPI(path, "POST", queryParams, postBody, headerParams, formParams, contentType, null);
  }
  
  /**
   * Deletes a pet
   * 
   * @param apiKey 
   * @param petId Pet id to delete
   * @return void
   */
  public void deletePet(String apiKey, Long petId) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
       throw new ApiException(400, "Missing the required parameter 'petId' when calling deletePet");
    }
    

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    Map<String, Object> queryParams = new HashMap<String, Object>();

    Map<String, Object> headerParams = new HashMap<String, Object>();
    headerParams.put("api_key", apiKey);

    Map<String, Object> formParams = new HashMap<String, Object>();

    String[] contentTypes = {
      
    };

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    apiClient.invokeAPI(path, "DELETE", queryParams, postBody, headerParams, formParams, contentType, null);
  }
  
  /**
   * uploads an image
   * 
   * @param petId ID of pet to update
   * @param additionalMetadata Additional data to pass to server
   * @param file file to upload
   * @return void
   */
  public void uploadFile(Long petId, String additionalMetadata, File file) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
       throw new ApiException(400, "Missing the required parameter 'petId' when calling uploadFile");
    }
    

    // create path and map variables
    String path = "/pet/{petId}/uploadImage".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    Map<String, Object> queryParams = new HashMap<String, Object>();

    Map<String, Object> headerParams = new HashMap<String, Object>();

    Map<String, Object> formParams = new HashMap<String, Object>();
    formParams.put("additionalMetadata", additionalMetadata);
    formParams.put("file", file);

    String[] contentTypes = {
      "multipart/form-data"
    };

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    apiClient.invokeAPI(path, "POST", queryParams, postBody, headerParams, formParams, contentType, null);
  }
  
}
