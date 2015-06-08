using System;
using System.Collections.Generic;
using io.swagger.client;
using io.swagger.Model;





namespace io.swagger.Api {
  
  public class StoreApi {
    private readonly ApiInvoker _apiInvoker = ApiInvoker.GetInstance();

    public StoreApi(String basePath = "http://petstore.swagger.io/v2")
    {
      BasePath = basePath;
    }

    public ApiInvoker GetInvoker() {
      return _apiInvoker;
    }

    public string BasePath { get; set; }

    

    /// <summary>
    /// Returns pet inventories by status Returns a map of status codes to quantities
    /// </summary>
    
    /// <returns></returns>
    public Dictionary<String, int?>  getInventory () {
      // create path and map variables
      var path = "/store/inventory".Replace("{format}","json");

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      

      

      try {
        if (typeof(Dictionary<String, int?>) == typeof(byte[])) {
          
          var response = _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return ((object)response) as Dictionary<String, int?>;
          
          
        } else {
          
          var response = _apiInvoker.InvokeApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          if (response != null){
             return (Dictionary<String, int?>) ApiInvoker.Deserialize(response, typeof(Dictionary<String, int?>));
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
    /// Place an order for a pet 
    /// </summary>
    /// <param name="Body">order placed for purchasing the pet</param>
    
    /// <returns></returns>
    public Order  placeOrder (Order Body) {
      // create path and map variables
      var path = "/store/order".Replace("{format}","json");

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      

      

      try {
        if (typeof(Order) == typeof(byte[])) {
          
          var response = _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return ((object)response) as Order;
          
          
        } else {
          
          var response = _apiInvoker.InvokeApi(BasePath, path, "POST", queryParams, Body, headerParams, formParams);
          if (response != null){
             return (Order) ApiInvoker.Deserialize(response, typeof(Order));
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
    /// Find purchase order by ID For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
    /// </summary>
    /// <param name="OrderId">ID of pet that needs to be fetched</param>
    
    /// <returns></returns>
    public Order  getOrderById (string OrderId) {
      // create path and map variables
      var path = "/store/order/{orderId}".Replace("{format}","json").Replace("{" + "orderId" + "}", _apiInvoker.EscapeString(OrderId.ToString()));

      // query params
      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, object>();

      

      

      

      

      try {
        if (typeof(Order) == typeof(byte[])) {
          
          var response = _apiInvoker.InvokeBinaryApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          return ((object)response) as Order;
          
          
        } else {
          
          var response = _apiInvoker.InvokeApi(BasePath, path, "GET", queryParams, null, headerParams, formParams);
          if (response != null){
             return (Order) ApiInvoker.Deserialize(response, typeof(Order));
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
    /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    /// </summary>
    /// <param name="OrderId">ID of the order that needs to be deleted</param>
    
    /// <returns></returns>
    public void  deleteOrder (string OrderId) {
      // create path and map variables
      var path = "/store/order/{orderId}".Replace("{format}","json").Replace("{" + "orderId" + "}", _apiInvoker.EscapeString(OrderId.ToString()));

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
