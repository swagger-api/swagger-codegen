# swagger.api.DefaultApi

## Load the API package
```dart
import 'package:swagger/api.dart';
```

All URIs are relative to */*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testMethod**](DefaultApi.md#testMethod) | **GET** /test | 

# **testMethod**
> List<Test> testMethod()



### Example
```dart
import 'package:swagger/api.dart';

var api_instance = new DefaultApi();

try {
    var result = api_instance.testMethod();
    print(result);
} catch (e) {
    print("Exception when calling DefaultApi->testMethod: $e\n");
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**List<Test>**](Test.md)

### Authorization

[bearer](../README.md#bearer)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

