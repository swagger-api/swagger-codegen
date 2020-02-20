# DefaultApi

All URIs are relative to */*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testMethod**](DefaultApi.md#testMethod) | **GET** /test | 

<a name="testMethod"></a>
# **testMethod**
> kotlin.Array&lt;Test&gt; testMethod()



### Example
```kotlin
// Import classes:
//import io.swagger.client.infrastructure.*
//import io.swagger.client.models.*;

val apiInstance = DefaultApi()
try {
    val result : kotlin.Array<Test> = apiInstance.testMethod()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling DefaultApi#testMethod")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DefaultApi#testMethod")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**kotlin.Array&lt;Test&gt;**](Test.md)

### Authorization

[bearer](../README.md#bearer)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

