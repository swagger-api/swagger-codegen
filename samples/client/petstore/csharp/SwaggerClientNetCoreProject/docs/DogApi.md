# IO.Swagger.Api.DogApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AddDog**](DogApi.md#adddog) | **POST** /dog | Add a new dog to the store
[**DeleteDog**](DogApi.md#deletedog) | **DELETE** /dog/{dogId} | Deletes a dog
[**GetDogById**](DogApi.md#getdogbyid) | **GET** /dog/{dogId} | Find dog by ID
[**UpdateDog**](DogApi.md#updatedog) | **PUT** /dog | Update an existing dog
[**UpdateDogWithForm**](DogApi.md#updatedogwithform) | **POST** /dog/{dogId} | Updates a dog

<a name="adddog"></a>
# **AddDog**
> void AddDog (Dog body)

Add a new dog to the store

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class AddDogExample
    {
        public void main()
        {
            var apiInstance = new DogApi();
            var body = new Dog(); // Dog | Dog object that needs to be added to the store

            try
            {
                // Add a new dog to the store
                apiInstance.AddDog(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling DogApi.AddDog: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Dog**](Dog.md)| Dog object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="deletedog"></a>
# **DeleteDog**
> void DeleteDog (long? dogId, string apiKey = null)

Deletes a dog

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class DeleteDogExample
    {
        public void main()
        {
            var apiInstance = new DogApi();
            var dogId = 789;  // long? | Dog id to delete
            var apiKey = apiKey_example;  // string |  (optional) 

            try
            {
                // Deletes a dog
                apiInstance.DeleteDog(dogId, apiKey);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling DogApi.DeleteDog: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **dogId** | **long?**| Dog id to delete | 
 **apiKey** | **string**|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="getdogbyid"></a>
# **GetDogById**
> Dog GetDogById (long? dogId)

Find dog by ID

Returns a single dog

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class GetDogByIdExample
    {
        public void main()
        {
            var apiInstance = new DogApi();
            var dogId = 789;  // long? | ID of dog to return

            try
            {
                // Find dog by ID
                Dog result = apiInstance.GetDogById(dogId);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling DogApi.GetDogById: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **dogId** | **long?**| ID of dog to return | 

### Return type

[**Dog**](Dog.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="updatedog"></a>
# **UpdateDog**
> void UpdateDog (Dog body)

Update an existing dog

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class UpdateDogExample
    {
        public void main()
        {
            var apiInstance = new DogApi();
            var body = new Dog(); // Dog | Dog object that needs to be added.

            try
            {
                // Update an existing dog
                apiInstance.UpdateDog(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling DogApi.UpdateDog: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Dog**](Dog.md)| Dog object that needs to be added. | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="updatedogwithform"></a>
# **UpdateDogWithForm**
> void UpdateDogWithForm (long? dogId, string name = null, string status = null)

Updates a dog

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class UpdateDogWithFormExample
    {
        public void main()
        {
            var apiInstance = new DogApi();
            var dogId = 789;  // long? | ID of dog that needs to be updated
            var name = name_example;  // string |  (optional) 
            var status = status_example;  // string |  (optional) 

            try
            {
                // Updates a dog
                apiInstance.UpdateDogWithForm(dogId, name, status);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling DogApi.UpdateDogWithForm: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **dogId** | **long?**| ID of dog that needs to be updated | 
 **name** | **string**|  | [optional] 
 **status** | **string**|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
