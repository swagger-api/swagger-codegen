# FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | 
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters
[**testEnumRequestBody**](FakeApi.md#testEnumRequestBody) | **POST** /fake/enum/form | To test enum parameters
[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **POST** /fake/jsonFormData | test json serialization of form data

<a name="fakeOuterBooleanSerialize"></a>
# **fakeOuterBooleanSerialize**
> Boolean fakeOuterBooleanSerialize(body)



Test serialization of outer boolean types

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
Boolean body = true; // Boolean | Input boolean as post body
try {
    Boolean result = apiInstance.fakeOuterBooleanSerialize(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#fakeOuterBooleanSerialize");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Boolean**](Boolean.md)| Input boolean as post body | [optional]

### Return type

**Boolean**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

<a name="fakeOuterCompositeSerialize"></a>
# **fakeOuterCompositeSerialize**
> OuterComposite fakeOuterCompositeSerialize(body)



Test serialization of object with outer number type

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
OuterComposite body = new OuterComposite(); // OuterComposite | Input composite as post body
try {
    OuterComposite result = apiInstance.fakeOuterCompositeSerialize(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#fakeOuterCompositeSerialize");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional]

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

<a name="fakeOuterNumberSerialize"></a>
# **fakeOuterNumberSerialize**
> BigDecimal fakeOuterNumberSerialize(body)



Test serialization of outer number types

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
BigDecimal body = new BigDecimal(); // BigDecimal | Input number as post body
try {
    BigDecimal result = apiInstance.fakeOuterNumberSerialize(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#fakeOuterNumberSerialize");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**BigDecimal**](BigDecimal.md)| Input number as post body | [optional]

### Return type

[**BigDecimal**](BigDecimal.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

<a name="fakeOuterStringSerialize"></a>
# **fakeOuterStringSerialize**
> String fakeOuterStringSerialize(body)



Test serialization of outer string types

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
String body = "body_example"; // String | Input string as post body
try {
    String result = apiInstance.fakeOuterStringSerialize(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#fakeOuterStringSerialize");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**String**](String.md)| Input string as post body | [optional]

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

<a name="testClientModel"></a>
# **testClientModel**
> Client testClientModel(body)

To test \&quot;client\&quot; model

To test \&quot;client\&quot; model

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
Client body = new Client(); // Client | client model
try {
    Client result = apiInstance.testClientModel(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testClientModel");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md)| client model |

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="testEndpointParameters"></a>
# **testEndpointParameters**
> testEndpointParameters(body)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```java
// Import classes:
//import io.swagger.client.ApiClient;
//import io.swagger.client.ApiException;
//import io.swagger.client.Configuration;
//import io.swagger.client.auth.*;
//import io.swagger.client.api.FakeApi;

ApiClient defaultClient = Configuration.getDefaultApiClient();
// Configure HTTP basic authorization: http_basic_test
HttpBasicAuth http_basic_test = (HttpBasicAuth) defaultClient.getAuthentication("http_basic_test");
http_basic_test.setUsername("YOUR USERNAME");
http_basic_test.setPassword("YOUR PASSWORD");

FakeApi apiInstance = new FakeApi();
Body2 body = new Body2(); // Body2 | 
try {
    apiInstance.testEndpointParameters(body);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testEndpointParameters");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Body2**](Body2.md)|  |

### Return type

null (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: Not defined

<a name="testEnumParameters"></a>
# **testEnumParameters**
> testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger)

To test enum parameters

To test enum parameters

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
List<String> enumHeaderStringArray = Arrays.asList("enumHeaderStringArray_example"); // List<String> | Header parameter enum test (string array)
String enumHeaderString = "-efg"; // String | Header parameter enum test (string)
List<String> enumQueryStringArray = Arrays.asList("enumQueryStringArray_example"); // List<String> | Query parameter enum test (string array)
String enumQueryString = "-efg"; // String | Query parameter enum test (string)
Integer enumQueryInteger = 56; // Integer | Query parameter enum test (double)
try {
    apiInstance.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testEnumParameters");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**List&lt;String&gt;**](String.md)| Header parameter enum test (string array) | [optional] [enum: >, $]
 **enumHeaderString** | **String**| Header parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]
 **enumQueryStringArray** | [**List&lt;String&gt;**](String.md)| Query parameter enum test (string array) | [optional] [enum: >, $]
 **enumQueryString** | **String**| Query parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]
 **enumQueryInteger** | **Integer**| Query parameter enum test (double) | [optional] [enum: 1, -2]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="testEnumRequestBody"></a>
# **testEnumRequestBody**
> testEnumRequestBody(body)

To test enum parameters

To test enum parameters

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
Body4 body = new Body4(); // Body4 | 
try {
    apiInstance.testEnumRequestBody(body);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testEnumRequestBody");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Body4**](Body4.md)|  | [optional]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: Not defined

<a name="testInlineAdditionalProperties"></a>
# **testInlineAdditionalProperties**
> testInlineAdditionalProperties(body)

test inline additionalProperties

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
Map<String, String> body = new Map(); // Map<String, String> | request body
try {
    apiInstance.testInlineAdditionalProperties(body);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testInlineAdditionalProperties");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Map&lt;String, String&gt;**](Map.md)| request body |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a name="testJsonFormData"></a>
# **testJsonFormData**
> testJsonFormData(body)

test json serialization of form data

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
Body5 body = new Body5(); // Body5 | 
try {
    apiInstance.testJsonFormData(body);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testJsonFormData");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Body5**](Body5.md)|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

