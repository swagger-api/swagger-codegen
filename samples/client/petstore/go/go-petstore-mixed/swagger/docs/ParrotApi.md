# {{classname}}

All URIs are relative to */v3*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AddParrot**](ParrotApi.md#AddParrot) | **Post** /parrot | Add a new parrow to the store
[**GetParrots**](ParrotApi.md#GetParrots) | **Get** /parrot | get Parrots
[**UpdateParrots**](ParrotApi.md#UpdateParrots) | **Put** /parrot | update parrots

# **AddParrot**
> InlineResponse2001 AddParrot(ctx, optional)
Add a new parrow to the store

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
 **optional** | ***ParrotApiAddParrotOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a ParrotApiAddParrotOpts struct
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**optional.Interface of Body9**](Body9.md)|  | 

### Return type

[**InlineResponse2001**](inline_response_200_1.md)

### Authorization

[jwt](../README.md#jwt)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **GetParrots**
> []InlineResponseItems200 GetParrots(ctx, )
get Parrots

### Required Parameters
This endpoint does not need any parameter.

### Return type

[**[]InlineResponseItems200**](inline_response_items200.md)

### Authorization

[jwt](../README.md#jwt)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UpdateParrots**
> InlineResponse200 UpdateParrots(ctx, optional)
update parrots

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
 **optional** | ***ParrotApiUpdateParrotsOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a ParrotApiUpdateParrotsOpts struct
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**optional.Interface of Body8**](Body8.md)|  | 

### Return type

[**InlineResponse200**](inline_response_200.md)

### Authorization

[jwt](../README.md#jwt)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

