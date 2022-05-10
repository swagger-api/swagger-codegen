# IO.Swagger.Api.AnimalApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AddAnimal**](AnimalApi.md#addanimal) | **POST** /animal | Add a new animal to the store
[**DeleteAnimal**](AnimalApi.md#deleteanimal) | **DELETE** /animal/{animalId} | Deletes a animal
[**GetAnimalById**](AnimalApi.md#getanimalbyid) | **GET** /animal/{animalId} | Find animal by ID
[**UpdateAnimal**](AnimalApi.md#updateanimal) | **PUT** /animal | Update an existing animal
[**UpdateAnimalWithForm**](AnimalApi.md#updateanimalwithform) | **POST** /animal/{animalId} | Updates a animal

<a name="addanimal"></a>
# **AddAnimal**
> void AddAnimal (Animal body)

Add a new animal to the store

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class AddAnimalExample
    {
        public void main()
        {
            var apiInstance = new AnimalApi();
            var body = new Animal(); // Animal | Animal object that needs to be added to the store

            try
            {
                // Add a new animal to the store
                apiInstance.AddAnimal(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling AnimalApi.AddAnimal: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Animal**](Animal.md)| Animal object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="deleteanimal"></a>
# **DeleteAnimal**
> void DeleteAnimal (long? animalId, string apiKey = null)

Deletes a animal

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class DeleteAnimalExample
    {
        public void main()
        {
            var apiInstance = new AnimalApi();
            var animalId = 789;  // long? | Animal id to delete
            var apiKey = apiKey_example;  // string |  (optional) 

            try
            {
                // Deletes a animal
                apiInstance.DeleteAnimal(animalId, apiKey);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling AnimalApi.DeleteAnimal: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **animalId** | **long?**| Animal id to delete | 
 **apiKey** | **string**|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="getanimalbyid"></a>
# **GetAnimalById**
> Animal GetAnimalById (long? animalId)

Find animal by ID

Returns a single animal

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class GetAnimalByIdExample
    {
        public void main()
        {
            var apiInstance = new AnimalApi();
            var animalId = 789;  // long? | ID of pet to return

            try
            {
                // Find animal by ID
                Animal result = apiInstance.GetAnimalById(animalId);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling AnimalApi.GetAnimalById: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **animalId** | **long?**| ID of pet to return | 

### Return type

[**Animal**](Animal.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="updateanimal"></a>
# **UpdateAnimal**
> void UpdateAnimal (Animal body)

Update an existing animal

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class UpdateAnimalExample
    {
        public void main()
        {
            var apiInstance = new AnimalApi();
            var body = new Animal(); // Animal | Animal object that needs to be added.

            try
            {
                // Update an existing animal
                apiInstance.UpdateAnimal(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling AnimalApi.UpdateAnimal: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Animal**](Animal.md)| Animal object that needs to be added. | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="updateanimalwithform"></a>
# **UpdateAnimalWithForm**
> void UpdateAnimalWithForm (long? animalId, string name = null, string status = null)

Updates a animal

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class UpdateAnimalWithFormExample
    {
        public void main()
        {
            var apiInstance = new AnimalApi();
            var animalId = 789;  // long? | ID of animal that needs to be updated
            var name = name_example;  // string |  (optional) 
            var status = status_example;  // string |  (optional) 

            try
            {
                // Updates a animal
                apiInstance.UpdateAnimalWithForm(animalId, name, status);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling AnimalApi.UpdateAnimalWithForm: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **animalId** | **long?**| ID of animal that needs to be updated | 
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
