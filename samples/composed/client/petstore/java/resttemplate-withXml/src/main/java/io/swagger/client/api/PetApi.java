package io.swagger.client.api;

import io.swagger.client.ApiClient;

import io.swagger.client.model.Body1;
import io.swagger.client.model.Body2;
import io.swagger.client.model.InlineResponse200;
import io.swagger.client.model.InlineResponse2001;
import io.swagger.client.model.ModelApiResponse;
import io.swagger.client.model.Pet;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.util.UriComponentsBuilder;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

@Component("io.swagger.client.api.PetApi")
public class PetApi {
    private ApiClient apiClient;

    public PetApi() {
        this(new ApiClient());
    }

    @Autowired
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
     * Add a new parrow to the store
     * 
     * <p><b>405</b> - Invalid input
     * <p><b>200</b> - successful operation
     * @param body The body parameter
     * @return InlineResponse2001
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public InlineResponse2001 addParrot(Body2 body) throws RestClientException {
        Object postBody = body;
        String path = UriComponentsBuilder.fromPath("/parrot").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { 
            "application/json"
         };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/json"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<InlineResponse2001> returnType = new ParameterizedTypeReference<InlineResponse2001>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * Add a new pet to the store
     * 
     * <p><b>405</b> - Invalid input
     * @param body Pet object that needs to be added to the store
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void addPet(Pet body) throws RestClientException {
        Object postBody = body;
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'body' when calling addPet");
        }
        String path = UriComponentsBuilder.fromPath("/pet").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = {  };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/json", "application/xml"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * Deletes a pet
     * 
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Pet not found
     * @param petId Pet id to delete
     * @param apiKey The apiKey parameter
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void deletePet(Long petId, String apiKey) throws RestClientException {
        Object postBody = null;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'petId' when calling deletePet");
        }
        // create path and map variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        String path = UriComponentsBuilder.fromPath("/pet/{petId}").buildAndExpand(uriVariables).toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
        if (apiKey != null)
            headerParams.add("api_key", apiClient.parameterToString(apiKey));

        final String[] accepts = {  };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = {  };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        apiClient.invokeAPI(path, HttpMethod.DELETE, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * Find pet by ID
     * schedule pet feeding
     * <p><b>200</b> - successful operation
     * @param body Pet object that needs to be added to the store
     * @param token status
     * @param petType type of food
     * @param status status
     * @param petId ID of pet to return
     * @param sessionId session id
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void feedPet(Pet body, String token, String petType, String status, Long petId, String sessionId) throws RestClientException {
        Object postBody = body;
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'body' when calling feedPet");
        }
        // verify the required parameter 'token' is set
        if (token == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'token' when calling feedPet");
        }
        // verify the required parameter 'petType' is set
        if (petType == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'petType' when calling feedPet");
        }
        // verify the required parameter 'status' is set
        if (status == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'status' when calling feedPet");
        }
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'petId' when calling feedPet");
        }
        // verify the required parameter 'sessionId' is set
        if (sessionId == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'sessionId' when calling feedPet");
        }
        // create path and map variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        String path = UriComponentsBuilder.fromPath("/pet/feed/{petId}").buildAndExpand(uriVariables).toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "petType", petType));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "status", status));
        if (token != null)
            headerParams.add("token", apiClient.parameterToString(token));

        final String[] accepts = {  };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/json", "application/xml"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid status value
     * @param status Status values that need to be considered for filter
     * @return List&lt;Pet&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public List<Pet> findPetsByStatus(List<String> status) throws RestClientException {
        Object postBody = null;
        // verify the required parameter 'status' is set
        if (status == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'status' when calling findPetsByStatus");
        }
        String path = UriComponentsBuilder.fromPath("/pet/findByStatus").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("multi".toUpperCase()), "status", status));

        final String[] accepts = { 
            "application/json", "application/xml"
         };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = {  };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<List<Pet>> returnType = new ParameterizedTypeReference<List<Pet>>() {};
        return apiClient.invokeAPI(path, HttpMethod.GET, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * Finds Pets by tags
     * Muliple tags can be provided with comma separated strings. Use\\ \\ tag1, tag2, tag3 for testing.
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid tag value
     * @param tags Tags to filter by
     * @return List&lt;Pet&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public List<Pet> findPetsByTags(List<String> tags) throws RestClientException {
        Object postBody = null;
        // verify the required parameter 'tags' is set
        if (tags == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'tags' when calling findPetsByTags");
        }
        String path = UriComponentsBuilder.fromPath("/pet/findByTags").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("multi".toUpperCase()), "tags", tags));

        final String[] accepts = { 
            "application/json", "application/xml"
         };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = {  };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<List<Pet>> returnType = new ParameterizedTypeReference<List<Pet>>() {};
        return apiClient.invokeAPI(path, HttpMethod.GET, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * get Parrots
     * 
     * <p><b>200</b> - successful operation
     * @return List&lt;Object&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public List<Object> getParrots() throws RestClientException {
        Object postBody = null;
        String path = UriComponentsBuilder.fromPath("/parrot").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { 
            "application/json"
         };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = {  };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<List<Object>> returnType = new ParameterizedTypeReference<List<Object>>() {};
        return apiClient.invokeAPI(path, HttpMethod.GET, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * Find pet by ID
     * Returns a single pet
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Pet not found
     * @param petId ID of pet to return
     * @return Pet
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public Pet getPetById(Long petId) throws RestClientException {
        Object postBody = null;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'petId' when calling getPetById");
        }
        // create path and map variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        String path = UriComponentsBuilder.fromPath("/pet/{petId}").buildAndExpand(uriVariables).toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { 
            "application/json", "application/xml"
         };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = {  };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] { "api_key" };

        ParameterizedTypeReference<Pet> returnType = new ParameterizedTypeReference<Pet>() {};
        return apiClient.invokeAPI(path, HttpMethod.GET, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * update parrots
     * 
     * <p><b>405</b> - Invalid input
     * <p><b>200</b> - successful operation
     * @param body The body parameter
     * @return InlineResponse200
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public InlineResponse200 updateParrots(Body1 body) throws RestClientException {
        Object postBody = body;
        String path = UriComponentsBuilder.fromPath("/parrot").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { 
            "application/json"
         };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/json"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<InlineResponse200> returnType = new ParameterizedTypeReference<InlineResponse200>() {};
        return apiClient.invokeAPI(path, HttpMethod.PUT, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * Update an existing pet
     * 
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Pet not found
     * <p><b>405</b> - Validation exception
     * @param body Pet object that needs to be added to the store
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void updatePet(Pet body) throws RestClientException {
        Object postBody = body;
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'body' when calling updatePet");
        }
        String path = UriComponentsBuilder.fromPath("/pet").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = {  };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/json", "application/xml"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        apiClient.invokeAPI(path, HttpMethod.PUT, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * Updates a pet in the store with form data
     * 
     * <p><b>405</b> - Invalid input
     * @param petId ID of pet that needs to be updated
     * @param name The name parameter
     * @param status The status parameter
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void updatePetWithForm(Long petId, String name, String status) throws RestClientException {
        Object postBody = null;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'petId' when calling updatePetWithForm");
        }
        // create path and map variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        String path = UriComponentsBuilder.fromPath("/pet/{petId}").buildAndExpand(uriVariables).toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
        if (name != null)
            formParams.add("name", name);
        if (status != null)
            formParams.add("status", status);

        final String[] accepts = {  };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/x-www-form-urlencoded"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * uploads an image
     * 
     * <p><b>200</b> - successful operation
     * @param petId ID of pet to update
     * @param body The body parameter
     * @return ModelApiResponse
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ModelApiResponse uploadFile(Long petId, Object body) throws RestClientException {
        Object postBody = body;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'petId' when calling uploadFile");
        }
        // create path and map variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        String path = UriComponentsBuilder.fromPath("/pet/{petId}/uploadImage").buildAndExpand(uriVariables).toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { 
            "application/json"
         };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/octet-stream"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<ModelApiResponse> returnType = new ParameterizedTypeReference<ModelApiResponse>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
}
