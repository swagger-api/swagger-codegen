# DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testBodyWithQueryParams**](DefaultApi.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params | 


<a name="testBodyWithQueryParams"></a>
# **testBodyWithQueryParams**
> testBodyWithQueryParams(body, query)



### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.DefaultApi;


DefaultApi apiInstance = new DefaultApi();
User body = new User(); // User | 
String query = "query_example"; // String | 
try {
    apiInstance.testBodyWithQueryParams(body, query);
} catch (ApiException e) {
    System.err.println("Exception when calling DefaultApi#testBodyWithQueryParams");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**User**](User.md)|  |
 **query** | **String**|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

