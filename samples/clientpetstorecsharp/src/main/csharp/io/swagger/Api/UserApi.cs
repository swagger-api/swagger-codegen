using System;
using System.Collections.Generic;
using io.swagger.client;
using io.swagger.Model;





namespace io.swagger.Api {
  
  public class UserApi {
    private readonly ApiInvoker _apiInvoker = ApiInvoker.GetInstance();

    public UserApi(String basePath = "http://petstore.swagger.io/v2")
    {
      BasePath = basePath;
    }

    public ApiInvoker GetInvoker() {
      return _apiInvoker;
    }

    public string BasePath { get; set; }

    

    /// <summary>
    /// Create user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Body">Created user object</param>
    
    /// <returns></returns>
    public void  createUser (User Body) {
      // create path and map variables
      var path = "/user".Replace("{format}","json");

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      

      

      try {
        if (typeof(void) == typeof(byte[])) {
          
          
          _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return;
          
        } else {
          
          
          _apiInvoker.InvokeApi(BasePath, path, "POST", queryParams, Body, headerParams, formParams);
          return;
          
        }
      } catch (ApiException ex) {
        if(ex.ErrorCode == 404) {
          return ;
        }
        else {
          throw;
        }
      }
    }
    

    /// <summary>
    /// Creates list of users with given input array 
    /// </summary>
    /// <param name="Body">List of user object</param>
    
    /// <returns></returns>
    public void  createUsersWithArrayInput (List<User> Body) {
      // create path and map variables
      var path = "/user/createWithArray".Replace("{format}","json");

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      

      

      try {
        if (typeof(void) == typeof(byte[])) {
          
          
          _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return;
          
        } else {
          
          
          _apiInvoker.InvokeApi(BasePath, path, "POST", queryParams, Body, headerParams, formParams);
          return;
          
        }
      } catch (ApiException ex) {
        if(ex.ErrorCode == 404) {
          return ;
        }
        else {
          throw;
        }
      }
    }
    

    /// <summary>
    /// Creates list of users with given input array 
    /// </summary>
    /// <param name="Body">List of user object</param>
    
    /// <returns></returns>
    public void  createUsersWithListInput (List<User> Body) {
      // create path and map variables
      var path = "/user/createWithList".Replace("{format}","json");

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      

      

      try {
        if (typeof(void) == typeof(byte[])) {
          
          
          _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return;
          
        } else {
          
          
          _apiInvoker.InvokeApi(BasePath, path, "POST", queryParams, Body, headerParams, formParams);
          return;
          
        }
      } catch (ApiException ex) {
        if(ex.ErrorCode == 404) {
          return ;
        }
        else {
          throw;
        }
      }
    }
    

    /// <summary>
    /// Logs user into the system 
    /// </summary>
    /// <param name="Username">The user name for login</param>
     /// <param name="Password">The password for login in clear text</param>
    
    /// <returns></returns>
    public string  loginUser (string Username, string Password) {
      // create path and map variables
      var path = "/user/login".Replace("{format}","json");

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      if (Username != null){
        string paramStr = ApiInvoker.ParameterToQueryString(Username);
        queryParams.Add("username", paramStr);
      }
      if (Password != null){
        string paramStr = ApiInvoker.ParameterToQueryString(Password);
        queryParams.Add("password", paramStr);
      }
      

      

      

      try {
        if (typeof(string) == typeof(byte[])) {
          
          var response = _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return ((object)response) as string;
          
          
        } else {
          
          var response = _apiInvoker.InvokeApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          if (response != null){
             return (string) ApiInvoker.Deserialize(response, typeof(string));
          }
          else {
            return null;
          }
          
          
        }
      } catch (ApiException ex) {
        if(ex.ErrorCode == 404) {
          return null;
        }
        else {
          throw;
        }
      }
    }
    

    /// <summary>
    /// Logs out current logged in user session 
    /// </summary>
    
    /// <returns></returns>
    public void  logoutUser () {
      // create path and map variables
      var path = "/user/logout".Replace("{format}","json");

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      

      

      try {
        if (typeof(void) == typeof(byte[])) {
          
          
          _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return;
          
        } else {
          
          
          _apiInvoker.InvokeApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return;
          
        }
      } catch (ApiException ex) {
        if(ex.ErrorCode == 404) {
          return ;
        }
        else {
          throw;
        }
      }
    }
    

    /// <summary>
    /// Get user by user name 
    /// </summary>
    /// <param name="Username">The name that needs to be fetched. Use user1 for testing. </param>
    
    /// <returns></returns>
    public User  getUserByName (string Username) {
      // create path and map variables
      var path = "/user/{username}".Replace("{format}","json").Replace("{" + "username" + "}", _apiInvoker.EscapeString(Username.ToString()));

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      

      

      try {
        if (typeof(User) == typeof(byte[])) {
          
          var response = _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return ((object)response) as User;
          
          
        } else {
          
          var response = _apiInvoker.InvokeApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          if (response != null){
             return (User) ApiInvoker.Deserialize(response, typeof(User));
          }
          else {
            return null;
          }
          
          
        }
      } catch (ApiException ex) {
        if(ex.ErrorCode == 404) {
          return null;
        }
        else {
          throw;
        }
      }
    }
    

    /// <summary>
    /// Updated user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">name that need to be deleted</param>
     /// <param name="Body">Updated user object</param>
    
    /// <returns></returns>
    public void  updateUser (string Username, User Body) {
      // create path and map variables
      var path = "/user/{username}".Replace("{format}","json").Replace("{" + "username" + "}", _apiInvoker.EscapeString(Username.ToString()));

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      

      

      try {
        if (typeof(void) == typeof(byte[])) {
          
          
          _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return;
          
        } else {
          
          
          _apiInvoker.InvokeApi(BasePath, path, "PUT", queryParams, Body, headerParams, formParams);
          return;
          
        }
      } catch (ApiException ex) {
        if(ex.ErrorCode == 404) {
          return ;
        }
        else {
          throw;
        }
      }
    }
    

    /// <summary>
    /// Delete user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">The name that needs to be deleted</param>
    
    /// <returns></returns>
    public void  deleteUser (string Username) {
      // create path and map variables
      var path = "/user/{username}".Replace("{format}","json").Replace("{" + "username" + "}", _apiInvoker.EscapeString(Username.ToString()));

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      

      

      try {
        if (typeof(void) == typeof(byte[])) {
          
          
          _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return;
          
        } else {
          
          
          _apiInvoker.InvokeApi(BasePath, path, "DELETE", queryParams, null, headerParams, formParams);
          return;
          
        }
      } catch (ApiException ex) {
        if(ex.ErrorCode == 404) {
          return ;
        }
        else {
          throw;
        }
      }
    }
    
  }
  
}
