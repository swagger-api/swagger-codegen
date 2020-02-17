# DefaultApi

All URIs are relative to */*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testMethod**](DefaultApi.md#testMethod) | **GET** /test | 

<a name="testMethod"></a>
# **testMethod**
> List&lt;String&gt; testMethod()



### Example
```java
// Import classes:
//import io.swagger.client.ApiClient;
//import io.swagger.client.ApiException;
//import io.swagger.client.Configuration;
//import io.swagger.client.auth.*;
//import io.swagger.client.api.DefaultApi;

ApiClient defaultClient = Configuration.getDefaultApiClient();


DefaultApi apiInstance = new DefaultApi();
try {
    List<String> result = apiInstance.testMethod();
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling DefaultApi#testMethod");
    e.printStackTrace();
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**List&lt;String&gt;**

### Authorization

[bearer](../README.md#bearer)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

