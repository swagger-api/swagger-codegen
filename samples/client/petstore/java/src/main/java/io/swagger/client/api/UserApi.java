package io.swagger.client.api;

import io.swagger.client.ApiException;
import io.swagger.client.ApiClient;
import io.swagger.client.Configuration;

import io.swagger.client.model.*;

import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;

import java.util.*;

import io.swagger.client.model.User;
import java.util.*;

import java.io.File;
import java.util.Map;
import java.util.HashMap;

public class UserApi {
  private ApiClient apiClient;

  public UserApi() {
    this(Configuration.getDefaultApiClient());
  }

  public UserApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  
  /**
   * Create user
   * This can only be done by the logged in user.
   * @param body Created user object
   * @return void
   */
  public void createUser(User body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/user".replaceAll("\\{format\\}","json");

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
    apiClient.invokeAPI(path, "POST", queryParams, postBody, headerParams, formParams, null, authNames);
  }
  
  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object
   * @return void
   */
  public void createUsersWithArrayInput(List<User> body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/user/createWithArray".replaceAll("\\{format\\}","json");

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
    apiClient.invokeAPI(path, "POST", queryParams, postBody, headerParams, formParams, null, authNames);
  }
  
  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object
   * @return void
   */
  public void createUsersWithListInput(List<User> body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/user/createWithList".replaceAll("\\{format\\}","json");

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
    apiClient.invokeAPI(path, "POST", queryParams, postBody, headerParams, formParams, null, authNames);
  }
  
  /**
   * Logs user into the system
   * 
   * @param username The user name for login
   * @param password The password for login in clear text
   * @return String
   */
  public String loginUser(String username, String password) throws ApiException {
    Object postBody = null;
    

    // create path and map variables
    String path = "/user/login".replaceAll("\\{format\\}","json");

    Map<String, Object> queryParams = new HashMap<String, Object>();
    if (username != null)
      queryParams.put("username", username);
    if (password != null)
      queryParams.put("password", password);

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
    Type returnType = new TypeToken<String>(){}.getType();
    return apiClient.invokeAPI(path, "GET", queryParams, postBody, headerParams, formParams, returnType, authNames);
  }
  
  /**
   * Logs out current logged in user session
   * 
   * @return void
   */
  public void logoutUser() throws ApiException {
    Object postBody = null;
    

    // create path and map variables
    String path = "/user/logout".replaceAll("\\{format\\}","json");

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
    apiClient.invokeAPI(path, "GET", queryParams, postBody, headerParams, formParams, null, authNames);
  }
  
  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing. 
   * @return User
   */
  public User getUserByName(String username) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'username' is set
    if (username == null) {
       throw new ApiException(400, "Missing the required parameter 'username' when calling getUserByName");
    }
    

    // create path and map variables
    String path = "/user/{username}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "username" + "\\}", apiClient.escapeString(username.toString()));

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
    Type returnType = new TypeToken<User>(){}.getType();
    return apiClient.invokeAPI(path, "GET", queryParams, postBody, headerParams, formParams, returnType, authNames);
  }
  
  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted
   * @param body Updated user object
   * @return void
   */
  public void updateUser(String username, User body) throws ApiException {
    Object postBody = body;
    
    // verify the required parameter 'username' is set
    if (username == null) {
       throw new ApiException(400, "Missing the required parameter 'username' when calling updateUser");
    }
    

    // create path and map variables
    String path = "/user/{username}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "username" + "\\}", apiClient.escapeString(username.toString()));

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
    apiClient.invokeAPI(path, "PUT", queryParams, postBody, headerParams, formParams, null, authNames);
  }
  
  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted
   * @return void
   */
  public void deleteUser(String username) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'username' is set
    if (username == null) {
       throw new ApiException(400, "Missing the required parameter 'username' when calling deleteUser");
    }
    

    // create path and map variables
    String path = "/user/{username}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "username" + "\\}", apiClient.escapeString(username.toString()));

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
