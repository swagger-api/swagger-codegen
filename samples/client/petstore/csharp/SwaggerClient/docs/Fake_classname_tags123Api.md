# IO.Swagger.Api.Fake_classname_tags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestClassname**](Fake_classname_tags123Api.md#testclassname) | **PATCH** /fake_classname_test | To test class name in snake case


<a name="testclassname"></a>
# **TestClassname**
> IO.Swagger.Model.ModelClient TestClassname (IO.Swagger.Model.ModelClient body)

To test class name in snake case

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class TestClassnameExample
    {
        public void main()
        {
            var apiInstance = new Fake_classname_tags123Api();
            var body = new IO.Swagger.Model.ModelClient(); // IO.Swagger.Model.ModelClient | client model

            try
            {
                // To test class name in snake case
                IO.Swagger.Model.ModelClient result = apiInstance.TestClassname(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling Fake_classname_tags123Api.TestClassname: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**IO.Swagger.Model.ModelClient**](IO.Swagger.Model.ModelClient.md)| client model | 

### Return type

[**IO.Swagger.Model.ModelClient**](IO.Swagger.Model.ModelClient.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

