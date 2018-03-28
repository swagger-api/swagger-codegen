# FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClassname**](FakeClassnameTags123Api.md#testClassname) | **PATCH** /fake_classname_test | To test class name in snake case




<a name="testClassname"></a>
# **testClassname**
> Client testClassname(client)

To test class name in snake case

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.FakeClassnameTags123Api;



FakeClassnameTags123Api apiInstance = new FakeClassnameTags123Api();

Client client = ; // Client | client model

try {
    Client result = apiInstance.testClassname(client);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeClassnameTags123Api#testClassname");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**Client**](.md)| client model |


### Return type

[**Client**](Client.md)

### Authorization

[api_key_query](../README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json



