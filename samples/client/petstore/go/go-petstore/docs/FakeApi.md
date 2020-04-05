# {{classname}}

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**FakeOuterBooleanSerialize**](FakeApi.md#FakeOuterBooleanSerialize) | **Post** /fake/outer/boolean | 
[**FakeOuterCompositeSerialize**](FakeApi.md#FakeOuterCompositeSerialize) | **Post** /fake/outer/composite | 
[**FakeOuterNumberSerialize**](FakeApi.md#FakeOuterNumberSerialize) | **Post** /fake/outer/number | 
[**FakeOuterStringSerialize**](FakeApi.md#FakeOuterStringSerialize) | **Post** /fake/outer/string | 
[**TestClientModel**](FakeApi.md#TestClientModel) | **Patch** /fake | To test \&quot;client\&quot; model
[**TestEndpointParameters**](FakeApi.md#TestEndpointParameters) | **Post** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**TestEnumParameters**](FakeApi.md#TestEnumParameters) | **Get** /fake | To test enum parameters
[**TestEnumRequestBody**](FakeApi.md#TestEnumRequestBody) | **Post** /fake/enum/form | To test enum parameters
[**TestInlineAdditionalProperties**](FakeApi.md#TestInlineAdditionalProperties) | **Post** /fake/inline-additionalProperties | test inline additionalProperties
[**TestJsonFormData**](FakeApi.md#TestJsonFormData) | **Post** /fake/jsonFormData | test json serialization of form data

# **FakeOuterBooleanSerialize**
> bool FakeOuterBooleanSerialize(ctx, optional)


Test serialization of outer boolean types

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
 **optional** | ***FakeApiFakeOuterBooleanSerializeOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a FakeApiFakeOuterBooleanSerializeOpts struct
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**optional.Interface of bool**](bool.md)| Input boolean as post body | 

### Return type

**bool**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FakeOuterCompositeSerialize**
> OuterComposite FakeOuterCompositeSerialize(ctx, optional)


Test serialization of object with outer number type

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
 **optional** | ***FakeApiFakeOuterCompositeSerializeOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a FakeApiFakeOuterCompositeSerializeOpts struct
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**optional.Interface of OuterComposite**](OuterComposite.md)| Input composite as post body | 

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FakeOuterNumberSerialize**
> float64 FakeOuterNumberSerialize(ctx, optional)


Test serialization of outer number types

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
 **optional** | ***FakeApiFakeOuterNumberSerializeOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a FakeApiFakeOuterNumberSerializeOpts struct
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**optional.Interface of float64**](float64.md)| Input number as post body | 

### Return type

**float64**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FakeOuterStringSerialize**
> string FakeOuterStringSerialize(ctx, optional)


Test serialization of outer string types

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
 **optional** | ***FakeApiFakeOuterStringSerializeOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a FakeApiFakeOuterStringSerializeOpts struct
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**optional.Interface of string**](string.md)| Input string as post body | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestClientModel**
> Client TestClientModel(ctx, body)
To test \"client\" model

To test \"client\" model

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **body** | [**Client**](Client.md)| client model | 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestEndpointParameters**
> TestEndpointParameters(ctx, body)
Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **body** | [**Body2**](Body2.md)|  | 

### Return type

 (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestEnumParameters**
> TestEnumParameters(ctx, optional)
To test enum parameters

To test enum parameters

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
 **optional** | ***FakeApiTestEnumParametersOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a FakeApiTestEnumParametersOpts struct
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**optional.Interface of []string**](string.md)| Header parameter enum test (string array) | 
 **enumHeaderString** | **optional.String**| Header parameter enum test (string) | [default to -efg]
 **enumQueryStringArray** | [**optional.Interface of []string**](string.md)| Query parameter enum test (string array) | 
 **enumQueryString** | **optional.String**| Query parameter enum test (string) | [default to -efg]
 **enumQueryInteger** | **optional.Int32**| Query parameter enum test (double) | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestEnumRequestBody**
> TestEnumRequestBody(ctx, optional)
To test enum parameters

To test enum parameters

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
 **optional** | ***FakeApiTestEnumRequestBodyOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a FakeApiTestEnumRequestBodyOpts struct
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**optional.Interface of Body4**](Body4.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestInlineAdditionalProperties**
> TestInlineAdditionalProperties(ctx, body)
test inline additionalProperties

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **body** | [**map[string]string**](map.md)| request body | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestJsonFormData**
> TestJsonFormData(ctx, body)
test json serialization of form data

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **body** | [**Body5**](Body5.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

