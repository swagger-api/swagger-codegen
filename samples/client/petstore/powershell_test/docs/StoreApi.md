# IO.Swagger.IO.Swagger/API.StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteOrder**](StoreApi.md#deleteorder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**getInventory**](StoreApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status
[**getOrderById**](StoreApi.md#getorderbyid) | **GET** /store/order/{orderId} | Find purchase order by ID
[**placeOrder**](StoreApi.md#placeorder) | **POST** /store/order | Place an order for a pet


<a name="deleteorder"></a>
# **deleteOrder**
> void deleteOrder (String orderId)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class deleteOrderExample
    {
        public void main()
        {
            var apiInstance = new StoreApi();
            var orderId = orderId_example;  // String | ID of the order that needs to be deleted

            try
            {
                // Delete purchase order by ID
                apiInstance.deleteOrder(orderId);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling StoreApi.deleteOrder: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **String**| ID of the order that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="getinventory"></a>
# **getInventory**
> {String, Int32} getInventory ()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class getInventoryExample
    {
        public void main()
        {
            // Configure API key authorization: api_key
            Configuration.Default.ApiKey.Add("api_key", "YOUR_API_KEY");
            // Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add("api_key", "Bearer");

            var apiInstance = new StoreApi();

            try
            {
                // Returns pet inventories by status
                {String, Int32} result = apiInstance.getInventory();
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling StoreApi.getInventory: " + e.Message );
            }
        }
    }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**{String, Int32}**](CollectionsMap.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="getorderbyid"></a>
# **getOrderById**
> Order getOrderById (Int64 orderId)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class getOrderByIdExample
    {
        public void main()
        {
            var apiInstance = new StoreApi();
            var orderId = 789;  // Int64 | ID of pet that needs to be fetched

            try
            {
                // Find purchase order by ID
                Order result = apiInstance.getOrderById(orderId);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling StoreApi.getOrderById: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **Int64**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="placeorder"></a>
# **placeOrder**
> Order placeOrder (Order body)

Place an order for a pet



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class placeOrderExample
    {
        public void main()
        {
            var apiInstance = new StoreApi();
            var body = new Order(); // Order | order placed for purchasing the pet

            try
            {
                // Place an order for a pet
                Order result = apiInstance.placeOrder(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling StoreApi.placeOrder: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Order**](Order.md)| order placed for purchasing the pet | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

