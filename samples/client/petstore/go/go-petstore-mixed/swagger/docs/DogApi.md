# {{classname}}

All URIs are relative to */v3*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AddDog**](DogApi.md#AddDog) | **Post** /dog | Add a new dog to the store
[**DeleteDog**](DogApi.md#DeleteDog) | **Delete** /dog/{dogId} | Deletes a dog
[**GetDogById**](DogApi.md#GetDogById) | **Get** /dog/{dogId} | Find dog by ID
[**UpdateDog**](DogApi.md#UpdateDog) | **Put** /dog | Update an existing dog
[**UpdateDogWithForm**](DogApi.md#UpdateDogWithForm) | **Post** /dog/{dogId} | Updates a dog

# **AddDog**
> AddDog(ctx, body)
Add a new dog to the store

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **body** | [**Dog**](Dog.md)| Dog object that needs to be added to the store | 

### Return type

 (empty response body)

### Authorization

[http_bearer_test](../README.md#http_bearer_test)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **DeleteDog**
> DeleteDog(ctx, dogId, optional)
Deletes a dog

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **dogId** | **int64**| Dog id to delete | 
 **optional** | ***DogApiDeleteDogOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a DogApiDeleteDogOpts struct
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **apiKey** | **optional.String**|  | 

### Return type

 (empty response body)

### Authorization

[http_bearer_test](../README.md#http_bearer_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **GetDogById**
> Dog GetDogById(ctx, dogId)
Find dog by ID

Returns a single dog

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **dogId** | **int64**| ID of dog to return | 

### Return type

[**Dog**](Dog.md)

### Authorization

[http_bearer_test](../README.md#http_bearer_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UpdateDog**
> UpdateDog(ctx, body)
Update an existing dog

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **body** | [**Dog**](Dog.md)| Dog object that needs to be added. | 

### Return type

 (empty response body)

### Authorization

[http_bearer_test](../README.md#http_bearer_test)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UpdateDogWithForm**
> UpdateDogWithForm(ctx, dogId, optional)
Updates a dog

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **dogId** | **int64**| ID of dog that needs to be updated | 
 **optional** | ***DogApiUpdateDogWithFormOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a DogApiUpdateDogWithFormOpts struct
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **name** | **optional.**|  | 
 **status** | **optional.**|  | 

### Return type

 (empty response body)

### Authorization

[http_bearer_test](../README.md#http_bearer_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

