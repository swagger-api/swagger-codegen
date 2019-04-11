# IO.Swagger.Api.FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestClassname**](FakeClassnameTags123Api.md#testclassname) | **PATCH** /fake_classname_test | To test class name in snake case

<a name="testclassname"></a>
# **TestClassname**
> Client TestClassname (Client body)

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
            // Configure API key authorization: api_key_query
            Configuration.Default.AddApiKey("api_key_query", "YOUR_API_KEY");
            // Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
            // Configuration.Default.AddApiKeyPrefix("api_key_query", "Bearer");

            var apiInstance = new FakeClassnameTags123Api();
            var body = new Client(); // Client | client model

            try
            {
                // To test class name in snake case
                Client result = apiInstance.TestClassname(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeClassnameTags123Api.TestClassname: " + e.Message );
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

[api_key_query](../README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
