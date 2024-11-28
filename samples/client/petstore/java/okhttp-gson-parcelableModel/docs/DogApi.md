# DogApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addDog**](DogApi.md#addDog) | **POST** /dog | Add a new dog to the store
[**deleteDog**](DogApi.md#deleteDog) | **DELETE** /dog/{dogId} | Deletes a dog
[**getDogById**](DogApi.md#getDogById) | **GET** /dog/{dogId} | Find dog by ID
[**updateDog**](DogApi.md#updateDog) | **PUT** /dog | Update an existing dog
[**updateDogWithForm**](DogApi.md#updateDogWithForm) | **POST** /dog/{dogId} | Updates a dog

<a name="addDog"></a>
# **addDog**
> addDog(body)

Add a new dog to the store

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.DogApi;


DogApi apiInstance = new DogApi();
Dog body = new Dog(); // Dog | Dog object that needs to be added to the store
try {
    apiInstance.addDog(body);
} catch (ApiException e) {
    System.err.println("Exception when calling DogApi#addDog");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Dog**](Dog.md)| Dog object that needs to be added to the store |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

<a name="deleteDog"></a>
# **deleteDog**
> deleteDog(dogId, apiKey)

Deletes a dog

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.DogApi;


DogApi apiInstance = new DogApi();
Long dogId = 789L; // Long | Dog id to delete
String apiKey = "apiKey_example"; // String | 
try {
    apiInstance.deleteDog(dogId, apiKey);
} catch (ApiException e) {
    System.err.println("Exception when calling DogApi#deleteDog");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **dogId** | **Long**| Dog id to delete |
 **apiKey** | **String**|  | [optional]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="getDogById"></a>
# **getDogById**
> Dog getDogById(dogId)

Find dog by ID

Returns a single dog

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.DogApi;


DogApi apiInstance = new DogApi();
Long dogId = 789L; // Long | ID of dog to return
try {
    Dog result = apiInstance.getDogById(dogId);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling DogApi#getDogById");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **dogId** | **Long**| ID of dog to return |

### Return type

[**Dog**](Dog.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="updateDog"></a>
# **updateDog**
> updateDog(body)

Update an existing dog

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.DogApi;


DogApi apiInstance = new DogApi();
Dog body = new Dog(); // Dog | Dog object that needs to be added.
try {
    apiInstance.updateDog(body);
} catch (ApiException e) {
    System.err.println("Exception when calling DogApi#updateDog");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Dog**](Dog.md)| Dog object that needs to be added. |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

<a name="updateDogWithForm"></a>
# **updateDogWithForm**
> updateDogWithForm(dogId, name, status)

Updates a dog

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.DogApi;


DogApi apiInstance = new DogApi();
Long dogId = 789L; // Long | ID of dog that needs to be updated
String name = "name_example"; // String | 
String status = "status_example"; // String | 
try {
    apiInstance.updateDogWithForm(dogId, name, status);
} catch (ApiException e) {
    System.err.println("Exception when calling DogApi#updateDogWithForm");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **dogId** | **Long**| ID of dog that needs to be updated |
 **name** | **String**|  | [optional]
 **status** | **String**|  | [optional]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

