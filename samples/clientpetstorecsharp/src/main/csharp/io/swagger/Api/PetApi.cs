using System;
using System.Collections.Generic;
using io.swagger.client;
using io.swagger.Model;







namespace io.swagger.Api {
  
  public class PetApi {
    private readonly ApiInvoker _apiInvoker = ApiInvoker.GetInstance();

    public PetApi(String basePath = "http://petstore.swagger.io/v2")
    {
      BasePath = basePath;
    }

    public ApiInvoker GetInvoker() {
      return _apiInvoker;
    }

    public string BasePath { get; set; }

    

    /// <summary>
    /// Update an existing pet 
    /// </summary>
    /// <param name="Body">Pet object that needs to be added to the store</param>
    
    /// <returns></returns>
    public void  updatePet (Pet Body) {
      // create path and map variables
      var path = "/pet".Replace("{format}","json");

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
    /// Add a new pet to the store 
    /// </summary>
    /// <param name="Body">Pet object that needs to be added to the store</param>
    
    /// <returns></returns>
    public void  addPet (Pet Body) {
      // create path and map variables
      var path = "/pet".Replace("{format}","json");

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
    /// Finds Pets by status Multiple status values can be provided with comma seperated strings
    /// </summary>
    /// <param name="Status">Status values that need to be considered for filter</param>
    
    /// <returns></returns>
    public List<Pet>  findPetsByStatus (List<string> Status) {
      // create path and map variables
      var path = "/pet/findByStatus".Replace("{format}","json");

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      if (Status != null){
        string paramStr = ApiInvoker.ParameterToQueryString(Status);
        queryParams.Add("status", paramStr);
      }
      

      

      

      try {
        if (typeof(List<Pet>) == typeof(byte[])) {
          
          var response = _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return ((object)response) as List<Pet>;
          
          
        } else {
          
          var response = _apiInvoker.InvokeApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          if (response != null){
             return (List<Pet>) ApiInvoker.Deserialize(response, typeof(List<Pet>));
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
    /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    /// </summary>
    /// <param name="Tags">Tags to filter by</param>
    
    /// <returns></returns>
    public List<Pet>  findPetsByTags (List<string> Tags) {
      // create path and map variables
      var path = "/pet/findByTags".Replace("{format}","json");

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      if (Tags != null){
        string paramStr = ApiInvoker.ParameterToQueryString(Tags);
        queryParams.Add("tags", paramStr);
      }
      

      

      

      try {
        if (typeof(List<Pet>) == typeof(byte[])) {
          
          var response = _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return ((object)response) as List<Pet>;
          
          
        } else {
          
          var response = _apiInvoker.InvokeApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          if (response != null){
             return (List<Pet>) ApiInvoker.Deserialize(response, typeof(List<Pet>));
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
    /// Find pet by ID Returns a single pet
    /// </summary>
    /// <param name="PetId">ID of pet to return</param>
    
    /// <returns></returns>
    public Pet  getPetById (long? PetId) {
      // create path and map variables
      var path = "/pet/{petId}".Replace("{format}","json").Replace("{" + "petId" + "}", _apiInvoker.EscapeString(PetId.ToString()));

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      

      

      try {
        if (typeof(Pet) == typeof(byte[])) {
          
          var response = _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return ((object)response) as Pet;
          
          
        } else {
          
          var response = _apiInvoker.InvokeApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          if (response != null){
             return (Pet) ApiInvoker.Deserialize(response, typeof(Pet));
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
    /// Updates a pet in the store with form data 
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be updated</param>
     /// <param name="Name">Updated name of the pet</param>
     /// <param name="Status">Updated status of the pet</param>
    
    /// <returns></returns>
    public void  updatePetWithForm (long? PetId, string Name, string Status) {
      // create path and map variables
      var path = "/pet/{petId}".Replace("{format}","json").Replace("{" + "petId" + "}", _apiInvoker.EscapeString(PetId.ToString()));

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      

      if (Name != null){
        if(Name is byte[]) {
          formParams.Add("name", Name);
        } else {
          string paramStr = (Name is DateTime) ? ((DateTime)(object)Name).ToString("u") : Convert.ToString(Name);
          formParams.Add("name", paramStr);
        }
      }
      if (Status != null){
        if(Status is byte[]) {
          formParams.Add("status", Status);
        } else {
          string paramStr = (Status is DateTime) ? ((DateTime)(object)Status).ToString("u") : Convert.ToString(Status);
          formParams.Add("status", paramStr);
        }
      }
      

      try {
        if (typeof(void) == typeof(byte[])) {
          
          
          _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return;
          
        } else {
          
          
          _apiInvoker.InvokeApi(BasePath, path, "POST", queryParams, null, headerParams, formParams);
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
    /// Deletes a pet 
    /// </summary>
    /// <param name="ApiKey"></param>
     /// <param name="PetId">Pet id to delete</param>
    
    /// <returns></returns>
    public void  deletePet (string ApiKey, long? PetId) {
      // create path and map variables
      var path = "/pet/{petId}".Replace("{format}","json").Replace("{" + "petId" + "}", _apiInvoker.EscapeString(PetId.ToString()));

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      headerParams.Add("api_key", ApiKey);
      

      

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
    

    /// <summary>
    /// uploads an image 
    /// </summary>
    /// <param name="PetId">ID of pet to update</param>
     /// <param name="AdditionalMetadata">Additional data to pass to server</param>
     /// <param name="File">file to upload</param>
    
    /// <returns></returns>
    public ApiResponse  uploadFile (long? PetId, string AdditionalMetadata, byte[] File) {
      // create path and map variables
      var path = "/pet/{petId}/uploadImage".Replace("{format}","json").Replace("{" + "petId" + "}", _apiInvoker.EscapeString(PetId.ToString()));

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      

      if (AdditionalMetadata != null){
        if(AdditionalMetadata is byte[]) {
          formParams.Add("additionalMetadata", AdditionalMetadata);
        } else {
          string paramStr = (AdditionalMetadata is DateTime) ? ((DateTime)(object)AdditionalMetadata).ToString("u") : Convert.ToString(AdditionalMetadata);
          formParams.Add("additionalMetadata", paramStr);
        }
      }
      if (File != null){
        if(File is byte[]) {
          formParams.Add("file", File);
        } else {
          string paramStr = (File is DateTime) ? ((DateTime)(object)File).ToString("u") : Convert.ToString(File);
          formParams.Add("file", paramStr);
        }
      }
      

      try {
        if (typeof(ApiResponse) == typeof(byte[])) {
          
          var response = _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return ((object)response) as ApiResponse;
          
          
        } else {
          
          var response = _apiInvoker.InvokeApi(BasePath, path, "POST", queryParams, null, headerParams, formParams);
          if (response != null){
             return (ApiResponse) ApiInvoker.Deserialize(response, typeof(ApiResponse));
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
    
  }
  
}
