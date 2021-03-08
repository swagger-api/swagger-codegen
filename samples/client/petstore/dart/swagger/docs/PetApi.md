# swagger.api.PetApi

## Load the API package
```dart
import 'package:swagger/api.dart';
```

All URIs are relative to */*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](PetApi.md#addPet) | **POST** /pet | Add a new pet to the store
[**deletePet**](PetApi.md#deletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**doCategoryStuff**](PetApi.md#doCategoryStuff) | **POST** /pet/category | 
[**findPetsByStatus**](PetApi.md#findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](PetApi.md#findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**getAllPets**](PetApi.md#getAllPets) | **GET** /allPets | 
[**getPetById**](PetApi.md#getPetById) | **GET** /pet/{petId} | Find pet by ID
[**getRandomPet**](PetApi.md#getRandomPet) | **GET** /randomPet | 
[**updatePet**](PetApi.md#updatePet) | **PUT** /pet | Update an existing pet
[**updatePetWithForm**](PetApi.md#updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](PetApi.md#uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image

# **addPet**
> addPet(body)

Add a new pet to the store

### Example
```dart
import 'package:swagger/api.dart';
// TODO Configure OAuth2 access token for authorization: petstore_auth
//swagger.api.Configuration.accessToken = 'YOUR_ACCESS_TOKEN';

var api_instance = new PetApi();
var body = new Pet(); // Pet | Pet object that needs to be added to the store

try {
    api_instance.addPet(body);
} catch (e) {
    print("Exception when calling PetApi->addPet: $e\n");
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
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deletePet**
> deletePet(petId, apiKey)

Deletes a pet

### Example
```dart
import 'package:swagger/api.dart';
// TODO Configure OAuth2 access token for authorization: petstore_auth
//swagger.api.Configuration.accessToken = 'YOUR_ACCESS_TOKEN';

var api_instance = new PetApi();
var petId = 789; // int | Pet id to delete
var apiKey = apiKey_example; // String | 

try {
    api_instance.deletePet(petId, apiKey);
} catch (e) {
    print("Exception when calling PetApi->deletePet: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| Pet id to delete | 
 **apiKey** | **String**|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **doCategoryStuff**
> ApiResponse doCategoryStuff(body)



### Example
```dart
import 'package:swagger/api.dart';

var api_instance = new PetApi();
var body = new SubCategory(); // SubCategory | 

try {
    var result = api_instance.doCategoryStuff(body);
    print(result);
} catch (e) {
    print("Exception when calling PetApi->doCategoryStuff: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**SubCategory**](SubCategory.md)|  | [optional] 

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByStatus**
> List<Pet> findPetsByStatus(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```dart
import 'package:swagger/api.dart';
// TODO Configure OAuth2 access token for authorization: petstore_auth
//swagger.api.Configuration.accessToken = 'YOUR_ACCESS_TOKEN';

var api_instance = new PetApi();
var status = []; // List<String> | Status values that need to be considered for filter

try {
    var result = api_instance.findPetsByStatus(status);
    print(result);
} catch (e) {
    print("Exception when calling PetApi->findPetsByStatus: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**List&lt;String&gt;**](String.md)| Status values that need to be considered for filter | 

### Return type

[**List<Pet>**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByTags**
> List<Pet> findPetsByTags(tags)

Finds Pets by tags

Muliple tags can be provided with comma separated strings. Use\\ \\ tag1, tag2, tag3 for testing.

### Example
```dart
import 'package:swagger/api.dart';
// TODO Configure OAuth2 access token for authorization: petstore_auth
//swagger.api.Configuration.accessToken = 'YOUR_ACCESS_TOKEN';

var api_instance = new PetApi();
var tags = []; // List<String> | Tags to filter by

try {
    var result = api_instance.findPetsByTags(tags);
    print(result);
} catch (e) {
    print("Exception when calling PetApi->findPetsByTags: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**List&lt;String&gt;**](String.md)| Tags to filter by | 

### Return type

[**List<Pet>**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getAllPets**
> AllPetsResponse getAllPets()



### Example
```dart
import 'package:swagger/api.dart';

var api_instance = new PetApi();

try {
    var result = api_instance.getAllPets();
    print(result);
} catch (e) {
    print("Exception when calling PetApi->getAllPets: $e\n");
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**AllPetsResponse**](AllPetsResponse.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getPetById**
> Pet getPetById(petId)

Find pet by ID

Returns a single pet

### Example
```dart
import 'package:swagger/api.dart';
// TODO Configure API key authorization: api_key
//swagger.api.Configuration.apiKey{'api_key'} = 'YOUR_API_KEY';
// uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//swagger.api.Configuration.apiKeyPrefix{'api_key'} = "Bearer";

var api_instance = new PetApi();
var petId = 789; // int | ID of pet to return

try {
    var result = api_instance.getPetById(petId);
    print(result);
} catch (e) {
    print("Exception when calling PetApi->getPetById: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| ID of pet to return | 

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getRandomPet**
> SinglePetResponse getRandomPet()



### Example
```dart
import 'package:swagger/api.dart';

var api_instance = new PetApi();

try {
    var result = api_instance.getRandomPet();
    print(result);
} catch (e) {
    print("Exception when calling PetApi->getRandomPet: $e\n");
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**SinglePetResponse**](SinglePetResponse.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePet**
> updatePet(body)

Update an existing pet

### Example
```dart
import 'package:swagger/api.dart';
// TODO Configure OAuth2 access token for authorization: petstore_auth
//swagger.api.Configuration.accessToken = 'YOUR_ACCESS_TOKEN';

var api_instance = new PetApi();
var body = new Pet(); // Pet | Pet object that needs to be added to the store

try {
    api_instance.updatePet(body);
} catch (e) {
    print("Exception when calling PetApi->updatePet: $e\n");
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
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePetWithForm**
> updatePetWithForm(petId, name, status)

Updates a pet in the store with form data

### Example
```dart
import 'package:swagger/api.dart';
// TODO Configure OAuth2 access token for authorization: petstore_auth
//swagger.api.Configuration.accessToken = 'YOUR_ACCESS_TOKEN';

var api_instance = new PetApi();
var petId = 789; // int | ID of pet that needs to be updated
var name = name_example; // String | 
var status = status_example; // String | 

try {
    api_instance.updatePetWithForm(petId, name, status);
} catch (e) {
    print("Exception when calling PetApi->updatePetWithForm: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| ID of pet that needs to be updated | 
 **name** | **String**|  | [optional] 
 **status** | **String**|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **uploadFile**
> ApiResponse uploadFile(petId, body)

uploads an image

### Example
```dart
import 'package:swagger/api.dart';
// TODO Configure OAuth2 access token for authorization: petstore_auth
//swagger.api.Configuration.accessToken = 'YOUR_ACCESS_TOKEN';

var api_instance = new PetApi();
var petId = 789; // int | ID of pet to update
var body = new Object(); // Object | 

try {
    var result = api_instance.uploadFile(petId, body);
    print(result);
} catch (e) {
    print("Exception when calling PetApi->uploadFile: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| ID of pet to update | 
 **body** | [**Object**](Object.md)|  | [optional] 

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/octet-stream
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

