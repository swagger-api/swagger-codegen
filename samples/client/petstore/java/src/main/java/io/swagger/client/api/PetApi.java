package io.swagger.client.api;

import io.swagger.client.ApiException;
import io.swagger.client.ApiClient;
import io.swagger.client.Configuration;

import io.swagger.client.model.*;

import java.util.*;

import io.swagger.client.model.Pet;
import java.io.File;

import org.glassfish.jersey.media.multipart.FormDataBodyPart;
import org.glassfish.jersey.media.multipart.FormDataMultiPart;
import org.glassfish.jersey.media.multipart.MultiPart;
import org.glassfish.jersey.media.multipart.file.FileDataBodyPart;

import javax.ws.rs.client.Entity;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.MediaType;

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
  public void updatePet (Pet body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/pet".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();
    Entity<?> formEntity = null;

    

    

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);

    final String[] contentTypes = {
      "application/json", "application/xml"
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);

    if(contentType.startsWith("multipart/form-data")) {
      boolean hasParams = false;
      MultiPart multipart = new MultiPart();

      
      if(hasParams)
        formEntity = Entity.entity(multipart, MediaType.MULTIPART_FORM_DATA_TYPE);
    }
    else {
      boolean hasValues = false;
      Form form = new Form();
      
      if(hasValues) {
        formEntity = Entity.entity(form, MediaType.APPLICATION_FORM_URLENCODED_TYPE);
      }
    }

    try {
      String[] authNames = new String[] { "petstore_auth" };
      String response = apiClient.invokeAPI(path, "PUT", queryParams, postBody, headerParams, formEntity, accept, contentType, authNames);
      if(response != null){
        return ;
      }
      else {
        return ;
      }
    } catch (ApiException ex) {
      throw ex;
    }
  }
  
  /**
   * Add a new pet to the store
   * 
   * @param body Pet object that needs to be added to the store
   * @return void
   */
  public void addPet (Pet body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/pet".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();
    Entity<?> formEntity = null;

    

    

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);

    final String[] contentTypes = {
      "application/json", "application/xml"
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);

    if(contentType.startsWith("multipart/form-data")) {
      boolean hasParams = false;
      MultiPart multipart = new MultiPart();

      
      if(hasParams)
        formEntity = Entity.entity(multipart, MediaType.MULTIPART_FORM_DATA_TYPE);
    }
    else {
      boolean hasValues = false;
      Form form = new Form();
      
      if(hasValues) {
        formEntity = Entity.entity(form, MediaType.APPLICATION_FORM_URLENCODED_TYPE);
      }
    }

    try {
      String[] authNames = new String[] { "petstore_auth" };
      String response = apiClient.invokeAPI(path, "POST", queryParams, postBody, headerParams, formEntity, accept, contentType, authNames);
      if(response != null){
        return ;
      }
      else {
        return ;
      }
    } catch (ApiException ex) {
      throw ex;
    }
  }
  
  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma seperated strings
   * @param status Status values that need to be considered for filter
   * @return List<Pet>
   */
  public List<Pet> findPetsByStatus (List<String> status) throws ApiException {
    Object postBody = null;
    

    // create path and map variables
    String path = "/pet/findByStatus".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();
    Entity<?> formEntity = null;

    if (status != null)
      queryParams.put("status", apiClient.parameterToString(status));
    

    

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);

    if(contentType.startsWith("multipart/form-data")) {
      boolean hasParams = false;
      MultiPart multipart = new MultiPart();

      
      if(hasParams)
        formEntity = Entity.entity(multipart, MediaType.MULTIPART_FORM_DATA_TYPE);
    }
    else {
      boolean hasValues = false;
      Form form = new Form();
      
      if(hasValues) {
        formEntity = Entity.entity(form, MediaType.APPLICATION_FORM_URLENCODED_TYPE);
      }
    }

    try {
      String[] authNames = new String[] { "petstore_auth" };
      String response = apiClient.invokeAPI(path, "GET", queryParams, postBody, headerParams, formEntity, accept, contentType, authNames);
      if(response != null){
        return (List<Pet>) apiClient.deserialize(response, "array", Pet.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      throw ex;
    }
  }
  
  /**
   * Finds Pets by tags
   * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by
   * @return List<Pet>
   */
  public List<Pet> findPetsByTags (List<String> tags) throws ApiException {
    Object postBody = null;
    

    // create path and map variables
    String path = "/pet/findByTags".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();
    Entity<?> formEntity = null;

    if (tags != null)
      queryParams.put("tags", apiClient.parameterToString(tags));
    

    

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);

    if(contentType.startsWith("multipart/form-data")) {
      boolean hasParams = false;
      MultiPart multipart = new MultiPart();

      
      if(hasParams)
        formEntity = Entity.entity(multipart, MediaType.MULTIPART_FORM_DATA_TYPE);
    }
    else {
      boolean hasValues = false;
      Form form = new Form();
      
      if(hasValues) {
        formEntity = Entity.entity(form, MediaType.APPLICATION_FORM_URLENCODED_TYPE);
      }
    }

    try {
      String[] authNames = new String[] { "petstore_auth" };
      String response = apiClient.invokeAPI(path, "GET", queryParams, postBody, headerParams, formEntity, accept, contentType, authNames);
      if(response != null){
        return (List<Pet>) apiClient.deserialize(response, "array", Pet.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      throw ex;
    }
  }
  
  /**
   * Find pet by ID
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched
   * @return Pet
   */
  public Pet getPetById (Long petId) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
       throw new ApiException(400, "Missing the required parameter 'petId' when calling getPetById");
    }
    

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();
    Entity<?> formEntity = null;

    

    

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);

    if(contentType.startsWith("multipart/form-data")) {
      boolean hasParams = false;
      MultiPart multipart = new MultiPart();

      
      if(hasParams)
        formEntity = Entity.entity(multipart, MediaType.MULTIPART_FORM_DATA_TYPE);
    }
    else {
      boolean hasValues = false;
      Form form = new Form();
      
      if(hasValues) {
        formEntity = Entity.entity(form, MediaType.APPLICATION_FORM_URLENCODED_TYPE);
      }
    }

    try {
      String[] authNames = new String[] { "api_key", "petstore_auth" };
      String response = apiClient.invokeAPI(path, "GET", queryParams, postBody, headerParams, formEntity, accept, contentType, authNames);
      if(response != null){
        return (Pet) apiClient.deserialize(response, "", Pet.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      throw ex;
    }
  }
  
  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated
   * @param name Updated name of the pet
   * @param status Updated status of the pet
   * @return void
   */
  public void updatePetWithForm (String petId, String name, String status) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
       throw new ApiException(400, "Missing the required parameter 'petId' when calling updatePetWithForm");
    }
    

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();
    Entity<?> formEntity = null;

    

    

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);

    final String[] contentTypes = {
      "application/x-www-form-urlencoded"
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);

    if(contentType.startsWith("multipart/form-data")) {
      boolean hasParams = false;
      MultiPart multipart = new MultiPart();

      
      if (name != null) {
        hasParams = true;

        FormDataMultiPart mp = new FormDataMultiPart();
        mp.bodyPart(new FormDataBodyPart("name", apiClient.parameterToString(name)));
        multipart.bodyPart(mp, MediaType.MULTIPART_FORM_DATA_TYPE);
      }
      
      if (status != null) {
        hasParams = true;

        FormDataMultiPart mp = new FormDataMultiPart();
        mp.bodyPart(new FormDataBodyPart("status", apiClient.parameterToString(status)));
        multipart.bodyPart(mp, MediaType.MULTIPART_FORM_DATA_TYPE);
      }
      
      if(hasParams)
        formEntity = Entity.entity(multipart, MediaType.MULTIPART_FORM_DATA_TYPE);
    }
    else {
      boolean hasValues = false;
      Form form = new Form();
      if (name != null) {
        form.param("name", apiClient.parameterToString(name));
        hasValues = true;
      }
      if (status != null) {
        form.param("status", apiClient.parameterToString(status));
        hasValues = true;
      }
      
      if(hasValues) {
        formEntity = Entity.entity(form, MediaType.APPLICATION_FORM_URLENCODED_TYPE);
      }
    }

    try {
      String[] authNames = new String[] { "petstore_auth" };
      String response = apiClient.invokeAPI(path, "POST", queryParams, postBody, headerParams, formEntity, accept, contentType, authNames);
      if(response != null){
        return ;
      }
      else {
        return ;
      }
    } catch (ApiException ex) {
      throw ex;
    }
  }
  
  /**
   * Deletes a pet
   * 
   * @param apiKey 
   * @param petId Pet id to delete
   * @return void
   */
  public void deletePet (String apiKey, Long petId) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
       throw new ApiException(400, "Missing the required parameter 'petId' when calling deletePet");
    }
    

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();
    Entity<?> formEntity = null;

    

    if (apiKey != null)
      headerParams.put("api_key", apiClient.parameterToString(apiKey));
    

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);

    if(contentType.startsWith("multipart/form-data")) {
      boolean hasParams = false;
      MultiPart multipart = new MultiPart();

      
      if(hasParams)
        formEntity = Entity.entity(multipart, MediaType.MULTIPART_FORM_DATA_TYPE);
    }
    else {
      boolean hasValues = false;
      Form form = new Form();
      
      if(hasValues) {
        formEntity = Entity.entity(form, MediaType.APPLICATION_FORM_URLENCODED_TYPE);
      }
    }

    try {
      String[] authNames = new String[] { "petstore_auth" };
      String response = apiClient.invokeAPI(path, "DELETE", queryParams, postBody, headerParams, formEntity, accept, contentType, authNames);
      if(response != null){
        return ;
      }
      else {
        return ;
      }
    } catch (ApiException ex) {
      throw ex;
    }
  }
  
  /**
   * uploads an image
   * 
   * @param petId ID of pet to update
   * @param additionalMetadata Additional data to pass to server
   * @param file file to upload
   * @return void
   */
  public void uploadFile (Long petId, String additionalMetadata, File file) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
       throw new ApiException(400, "Missing the required parameter 'petId' when calling uploadFile");
    }
    

    // create path and map variables
    String path = "/pet/{petId}/uploadImage".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();
    Entity<?> formEntity = null;

    

    

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);

    final String[] contentTypes = {
      "multipart/form-data"
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);

    if(contentType.startsWith("multipart/form-data")) {
      boolean hasParams = false;
      MultiPart multipart = new MultiPart();

      
      if (additionalMetadata != null) {
        hasParams = true;

        FormDataMultiPart mp = new FormDataMultiPart();
        mp.bodyPart(new FormDataBodyPart("additionalMetadata", apiClient.parameterToString(additionalMetadata)));
        multipart.bodyPart(mp, MediaType.MULTIPART_FORM_DATA_TYPE);
      }
      
      if (file != null) {
        hasParams = true;
        multipart.bodyPart(new FileDataBodyPart("file", new java.io.File(file.getName()), MediaType.APPLICATION_OCTET_STREAM_TYPE));
      }
      
      if(hasParams)
        formEntity = Entity.entity(multipart, MediaType.MULTIPART_FORM_DATA_TYPE);
    }
    else {
      boolean hasValues = false;
      Form form = new Form();
      if (additionalMetadata != null) {
        form.param("additionalMetadata", apiClient.parameterToString(additionalMetadata));
        hasValues = true;
      }
      
      
      if(hasValues) {
        formEntity = Entity.entity(form, MediaType.APPLICATION_FORM_URLENCODED_TYPE);
      }
    }

    try {
      String[] authNames = new String[] { "petstore_auth" };
      String response = apiClient.invokeAPI(path, "POST", queryParams, postBody, headerParams, formEntity, accept, contentType, authNames);
      if(response != null){
        return ;
      }
      else {
        return ;
      }
    } catch (ApiException ex) {
      throw ex;
    }
  }
  
}
