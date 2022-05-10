# {{classname}}

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AddAnimal**](AnimalApi.md#AddAnimal) | **Post** /animal | Add a new animal to the store
[**DeleteAnimal**](AnimalApi.md#DeleteAnimal) | **Delete** /animal/{animalId} | Deletes a animal
[**GetAnimalById**](AnimalApi.md#GetAnimalById) | **Get** /animal/{animalId} | Find animal by ID
[**UpdateAnimal**](AnimalApi.md#UpdateAnimal) | **Put** /animal | Update an existing animal
[**UpdateAnimalWithForm**](AnimalApi.md#UpdateAnimalWithForm) | **Post** /animal/{animalId} | Updates a animal

# **AddAnimal**
> AddAnimal(ctx, body)
Add a new animal to the store

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **body** | [**Animal**](Animal.md)| Animal object that needs to be added to the store | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **DeleteAnimal**
> DeleteAnimal(ctx, animalId, optional)
Deletes a animal

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **animalId** | **int64**| Animal id to delete | 
 **optional** | ***AnimalApiDeleteAnimalOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a AnimalApiDeleteAnimalOpts struct
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **apiKey** | **optional.String**|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **GetAnimalById**
> Animal GetAnimalById(ctx, animalId)
Find animal by ID

Returns a single animal

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **animalId** | **int64**| ID of pet to return | 

### Return type

[**Animal**](Animal.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UpdateAnimal**
> UpdateAnimal(ctx, body)
Update an existing animal

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **body** | [**Animal**](Animal.md)| Animal object that needs to be added. | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UpdateAnimalWithForm**
> UpdateAnimalWithForm(ctx, animalId, optional)
Updates a animal

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **animalId** | **int64**| ID of animal that needs to be updated | 
 **optional** | ***AnimalApiUpdateAnimalWithFormOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a AnimalApiUpdateAnimalWithFormOpts struct
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **name** | **optional.**|  | 
 **status** | **optional.**|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

