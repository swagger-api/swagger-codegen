# IO.Swagger.IO.Swagger/API.PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](PetApi.md#addpet) | **POST** /pet | Add a new pet to the store
[**deletePet**](PetApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
[**findPetsByStatus**](PetApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](PetApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
[**getPetById**](PetApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
[**updatePet**](PetApi.md#updatepet) | **PUT** /pet | Update an existing pet
[**updatePetWithForm**](PetApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](PetApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image


<a name="addpet"></a>
# **addPet**
> void addPet (Pet body)

Add a new pet to the store



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class addPetExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var body = new Pet(); // Pet | Pet object that needs to be added to the store

            try
            {
                // Add a new pet to the store
                apiInstance.addPet(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.addPet: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="deletepet"></a>
# **deletePet**
> void deletePet (Int64 petId, String apiKey)

Deletes a pet



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class deletePetExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var petId = 789;  // Int64 | Pet id to delete
            var apiKey = apiKey_example;  // String |  (optional) 

            try
            {
                // Deletes a pet
                apiInstance.deletePet(petId, apiKey);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.deletePet: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Int64**| Pet id to delete | 
 **apiKey** | **String**|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="findpetsbystatus"></a>
# **findPetsByStatus**
> [IO.Swagger.Model.Pet] findPetsByStatus ([String] status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class findPetsByStatusExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var status = new [String](); // [String] | Status values that need to be considered for filter

            try
            {
                // Finds Pets by status
                [IO.Swagger.Model.Pet] result = apiInstance.findPetsByStatus(status);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.findPetsByStatus: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**[String]**](String.md)| Status values that need to be considered for filter | 

### Return type

[**[IO.Swagger.Model.Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="findpetsbytags"></a>
# **findPetsByTags**
> [IO.Swagger.Model.Pet] findPetsByTags ([String] tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class findPetsByTagsExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var tags = new [String](); // [String] | Tags to filter by

            try
            {
                // Finds Pets by tags
                [IO.Swagger.Model.Pet] result = apiInstance.findPetsByTags(tags);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.findPetsByTags: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**[String]**](String.md)| Tags to filter by | 

### Return type

[**[IO.Swagger.Model.Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="getpetbyid"></a>
# **getPetById**
> IO.Swagger.Model.Pet getPetById (Int64 petId)

Find pet by ID

Returns a single pet

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class getPetByIdExample
    {
        public void main()
        {
            // Configure API key authorization: api_key
            Configuration.Default.ApiKey.Add("api_key", "YOUR_API_KEY");
            // Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add("api_key", "Bearer");

            var apiInstance = new PetApi();
            var petId = 789;  // Int64 | ID of pet to return

            try
            {
                // Find pet by ID
                IO.Swagger.Model.Pet result = apiInstance.getPetById(petId);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.getPetById: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Int64**| ID of pet to return | 

### Return type

[**IO.Swagger.Model.Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="updatepet"></a>
# **updatePet**
> void updatePet (Pet body)

Update an existing pet



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class updatePetExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var body = new Pet(); // Pet | Pet object that needs to be added to the store

            try
            {
                // Update an existing pet
                apiInstance.updatePet(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.updatePet: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="updatepetwithform"></a>
# **updatePetWithForm**
> void updatePetWithForm (Int64 petId, String name, String status)

Updates a pet in the store with form data



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class updatePetWithFormExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var petId = 789;  // Int64 | ID of pet that needs to be updated
            var name = name_example;  // String | Updated name of the pet (optional) 
            var status = status_example;  // String | Updated status of the pet (optional) 

            try
            {
                // Updates a pet in the store with form data
                apiInstance.updatePetWithForm(petId, name, status);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.updatePetWithForm: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Int64**| ID of pet that needs to be updated | 
 **name** | **String**| Updated name of the pet | [optional] 
 **status** | **String**| Updated status of the pet | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="uploadfile"></a>
# **uploadFile**
> IO.Swagger.Model.ApiResponse uploadFile (Int64 petId, String additionalMetadata, String file)

uploads an image



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class uploadFileExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var petId = 789;  // Int64 | ID of pet to update
            var additionalMetadata = additionalMetadata_example;  // String | Additional data to pass to server (optional) 
            var file = new String(); // String | file to upload (optional) 

            try
            {
                // uploads an image
                IO.Swagger.Model.ApiResponse result = apiInstance.uploadFile(petId, additionalMetadata, file);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.uploadFile: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Int64**| ID of pet to update | 
 **additionalMetadata** | **String**| Additional data to pass to server | [optional] 
 **file** | **String**| file to upload | [optional] 

### Return type

[**IO.Swagger.Model.ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

