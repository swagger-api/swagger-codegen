# AnimalApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addAnimal**](AnimalApi.md#addAnimal) | **POST** animal | Add a new animal to the store
[**deleteAnimal**](AnimalApi.md#deleteAnimal) | **DELETE** animal/{animalId} | Deletes a animal
[**getAnimalById**](AnimalApi.md#getAnimalById) | **GET** animal/{animalId} | Find animal by ID
[**updateAnimal**](AnimalApi.md#updateAnimal) | **PUT** animal | Update an existing animal
[**updateAnimalWithForm**](AnimalApi.md#updateAnimalWithForm) | **POST** animal/{animalId} | Updates a animal

<a name="addAnimal"></a>
# **addAnimal**
> Void addAnimal(body)

Add a new animal to the store

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.AnimalApi;


AnimalApi apiInstance = new AnimalApi();
Animal body = new Animal(); // Animal | Animal object that needs to be added to the store
try {
    Void result = apiInstance.addAnimal(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AnimalApi#addAnimal");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Animal**](Animal.md)| Animal object that needs to be added to the store |

### Return type

[**Void**](.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

<a name="deleteAnimal"></a>
# **deleteAnimal**
> Void deleteAnimal(animalId, apiKey)

Deletes a animal

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.AnimalApi;


AnimalApi apiInstance = new AnimalApi();
Long animalId = 789L; // Long | Animal id to delete
String apiKey = "apiKey_example"; // String | 
try {
    Void result = apiInstance.deleteAnimal(animalId, apiKey);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AnimalApi#deleteAnimal");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **animalId** | **Long**| Animal id to delete |
 **apiKey** | **String**|  | [optional]

### Return type

[**Void**](.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="getAnimalById"></a>
# **getAnimalById**
> Animal getAnimalById(animalId)

Find animal by ID

Returns a single animal

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.AnimalApi;


AnimalApi apiInstance = new AnimalApi();
Long animalId = 789L; // Long | ID of pet to return
try {
    Animal result = apiInstance.getAnimalById(animalId);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AnimalApi#getAnimalById");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **animalId** | **Long**| ID of pet to return |

### Return type

[**Animal**](Animal.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="updateAnimal"></a>
# **updateAnimal**
> Void updateAnimal(body)

Update an existing animal

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.AnimalApi;


AnimalApi apiInstance = new AnimalApi();
Animal body = new Animal(); // Animal | Animal object that needs to be added.
try {
    Void result = apiInstance.updateAnimal(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AnimalApi#updateAnimal");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Animal**](Animal.md)| Animal object that needs to be added. |

### Return type

[**Void**](.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

<a name="updateAnimalWithForm"></a>
# **updateAnimalWithForm**
> Void updateAnimalWithForm(animalId, name, status)

Updates a animal

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.AnimalApi;


AnimalApi apiInstance = new AnimalApi();
Long animalId = 789L; // Long | ID of animal that needs to be updated
String name = "name_example"; // String | 
String status = "status_example"; // String | 
try {
    Void result = apiInstance.updateAnimalWithForm(animalId, name, status);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AnimalApi#updateAnimalWithForm");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **animalId** | **Long**| ID of animal that needs to be updated |
 **name** | **String**|  | [optional]
 **status** | **String**|  | [optional]

### Return type

[**Void**](.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

