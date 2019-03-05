# IO.Swagger.Api.AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestSpecialTags**](AnotherFakeApi.md#testspecialtags) | **PATCH** /another-fake/dummy | To test special tags

<a name="testspecialtags"></a>
# **TestSpecialTags**
> Client TestSpecialTags (Client body)

To test special tags

To test special tags

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class TestSpecialTagsExample
    {
        public void main()
        {
            var apiInstance = new AnotherFakeApi();
            var body = new Client(); // Client | client model

            try
            {
                // To test special tags
                Client result = apiInstance.TestSpecialTags(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling AnotherFakeApi.TestSpecialTags: " + e.Message );
            }
        }
    }
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
