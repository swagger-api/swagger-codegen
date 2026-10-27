package io.swagger.client.api;

import io.swagger.client.ApiException;
import io.swagger.client.ApiClient;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;

import javax.ws.rs.core.GenericType;

import io.swagger.client.model.Animal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class AnimalApi {
  private ApiClient apiClient;

  public AnimalApi() {
    this(Configuration.getDefaultApiClient());
  }

  public AnimalApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }



  /**
   * Add a new animal to the store
   * 
   * @param body Animal object that needs to be added to the store (required)
   * @throws ApiException if fails to make API call
   */
  public void addAnimal(Animal body) throws ApiException {
    Object localVarPostBody = body;
    // verify the required parameter 'body' is set
    if (body == null) {
      throw new ApiException(400, "Missing the required parameter 'body' when calling addAnimal");
    }
    // create path and map variables
    String localVarPath = "/animal";

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
  public static class DeleteAnimalOptionals {
    public String apiKey() {
      return this.apiKey;
    }

    public DeleteAnimalOptionals apiKey(String apiKey) {
      this.apiKey = apiKey;
      return this;
    }

    private String apiKey = null;
  }

  /**
   * Deletes a animal
   * 
   * @param animalId Animal id to delete (required)
   * @throws ApiException if fails to make API call
   */
  public void deleteAnimal(Long animalId) throws ApiException {
    deleteAnimal(animalId, null);
  }

  /**
   * Deletes a animal
   * 
   * @param animalId Animal id to delete (required)
   * @param optionals An object containing the optional parameters for this API call.
   * @throws ApiException if fails to make API call
   */
  public void deleteAnimalOpts(Long animalId, DeleteAnimalOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new DeleteAnimalOptionals();
    }
    deleteAnimal(animalId, optionals.apiKey());
  }

  /**
   * Deletes a animal
   * 
   * @param animalId Animal id to delete (required)
   * @param apiKey  (optional)
   * @throws ApiException if fails to make API call
   */
  public void deleteAnimal(Long animalId, String apiKey) throws ApiException {
    Object localVarPostBody = null;
    // verify the required parameter 'animalId' is set
    if (animalId == null) {
      throw new ApiException(400, "Missing the required parameter 'animalId' when calling deleteAnimal");
    }
    // create path and map variables
    String localVarPath = "/animal/{animalId}"
      .replaceAll("\\{" + "animalId" + "\\}", apiClient.escapeString(animalId.toString()));

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
   * Find animal by ID
   * Returns a single animal
   * @param animalId ID of pet to return (required)
   * @return Animal
   * @throws ApiException if fails to make API call
   */
  public Animal getAnimalById(Long animalId) throws ApiException {
    Object localVarPostBody = null;
    // verify the required parameter 'animalId' is set
    if (animalId == null) {
      throw new ApiException(400, "Missing the required parameter 'animalId' when calling getAnimalById");
    }
    // create path and map variables
    String localVarPath = "/animal/{animalId}"
      .replaceAll("\\{" + "animalId" + "\\}", apiClient.escapeString(animalId.toString()));

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

    GenericType<Animal> localVarReturnType = new GenericType<Animal>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }


  /**
   * Update an existing animal
   * 
   * @param body Animal object that needs to be added. (required)
   * @throws ApiException if fails to make API call
   */
  public void updateAnimal(Animal body) throws ApiException {
    Object localVarPostBody = body;
    // verify the required parameter 'body' is set
    if (body == null) {
      throw new ApiException(400, "Missing the required parameter 'body' when calling updateAnimal");
    }
    // create path and map variables
    String localVarPath = "/animal";

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
  public static class UpdateAnimalWithFormOptionals {
    public String name() {
      return this.name;
    }

    public UpdateAnimalWithFormOptionals name(String name) {
      this.name = name;
      return this;
    }

    private String name = null;
    public String status() {
      return this.status;
    }

    public UpdateAnimalWithFormOptionals status(String status) {
      this.status = status;
      return this;
    }

    private String status = null;
  }

  /**
   * Updates a animal
   * 
   * @param animalId ID of animal that needs to be updated (required)
   * @throws ApiException if fails to make API call
   */
  public void updateAnimalWithForm(Long animalId) throws ApiException {
    updateAnimalWithForm(animalId, null, null);
  }

  /**
   * Updates a animal
   * 
   * @param animalId ID of animal that needs to be updated (required)
   * @param optionals An object containing the optional parameters for this API call.
   * @throws ApiException if fails to make API call
   */
  public void updateAnimalWithFormOpts(Long animalId, UpdateAnimalWithFormOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new UpdateAnimalWithFormOptionals();
    }
    updateAnimalWithForm(animalId, optionals.name(), optionals.status());
  }

  /**
   * Updates a animal
   * 
   * @param animalId ID of animal that needs to be updated (required)
   * @param name  (optional)
   * @param status  (optional)
   * @throws ApiException if fails to make API call
   */
  public void updateAnimalWithForm(Long animalId, String name, String status) throws ApiException {
    Object localVarPostBody = null;
    // verify the required parameter 'animalId' is set
    if (animalId == null) {
      throw new ApiException(400, "Missing the required parameter 'animalId' when calling updateAnimalWithForm");
    }
    // create path and map variables
    String localVarPath = "/animal/{animalId}"
      .replaceAll("\\{" + "animalId" + "\\}", apiClient.escapeString(animalId.toString()));

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
