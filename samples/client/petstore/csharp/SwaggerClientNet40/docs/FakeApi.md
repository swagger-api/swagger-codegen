# IO.Swagger.Api.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**FakeOuterBooleanSerialize**](FakeApi.md#fakeouterbooleanserialize) | **POST** /fake/outer/boolean | 
[**FakeOuterCompositeSerialize**](FakeApi.md#fakeoutercompositeserialize) | **POST** /fake/outer/composite | 
[**FakeOuterNumberSerialize**](FakeApi.md#fakeouternumberserialize) | **POST** /fake/outer/number | 
[**FakeOuterStringSerialize**](FakeApi.md#fakeouterstringserialize) | **POST** /fake/outer/string | 
[**TestClientModel**](FakeApi.md#testclientmodel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**TestEndpointParameters**](FakeApi.md#testendpointparameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**TestEnumParameters**](FakeApi.md#testenumparameters) | **GET** /fake | To test enum parameters
[**TestEnumRequestBody**](FakeApi.md#testenumrequestbody) | **POST** /fake/enum/form | To test enum parameters
[**TestInlineAdditionalProperties**](FakeApi.md#testinlineadditionalproperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**TestJsonFormData**](FakeApi.md#testjsonformdata) | **POST** /fake/jsonFormData | test json serialization of form data

<a name="fakeouterbooleanserialize"></a>
# **FakeOuterBooleanSerialize**
> OuterBoolean FakeOuterBooleanSerialize (bool? body = null)



Test serialization of outer boolean types

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class FakeOuterBooleanSerializeExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var body = new bool?(); // bool? | Input boolean as post body (optional) 

            try
            {
                OuterBoolean result = apiInstance.FakeOuterBooleanSerialize(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.FakeOuterBooleanSerialize: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**bool?**](bool?.md)| Input boolean as post body | [optional] 

### Return type

[**OuterBoolean**](OuterBoolean.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="fakeoutercompositeserialize"></a>
# **FakeOuterCompositeSerialize**
> OuterComposite FakeOuterCompositeSerialize (OuterComposite body = null)



Test serialization of object with outer number type

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class FakeOuterCompositeSerializeExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var body = new OuterComposite(); // OuterComposite | Input composite as post body (optional) 

            try
            {
                OuterComposite result = apiInstance.FakeOuterCompositeSerialize(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.FakeOuterCompositeSerialize: " + e.Message );
            }
        }
    }
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="fakeouternumberserialize"></a>
# **FakeOuterNumberSerialize**
> OuterNumber FakeOuterNumberSerialize (decimal? body = null)



Test serialization of outer number types

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class FakeOuterNumberSerializeExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var body = new decimal?(); // decimal? | Input number as post body (optional) 

            try
            {
                OuterNumber result = apiInstance.FakeOuterNumberSerialize(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.FakeOuterNumberSerialize: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**decimal?**](decimal?.md)| Input number as post body | [optional] 

### Return type

[**OuterNumber**](OuterNumber.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="fakeouterstringserialize"></a>
# **FakeOuterStringSerialize**
> OuterString FakeOuterStringSerialize (string body = null)



Test serialization of outer string types

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class FakeOuterStringSerializeExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var body = new string(); // string | Input string as post body (optional) 

            try
            {
                OuterString result = apiInstance.FakeOuterStringSerialize(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.FakeOuterStringSerialize: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**string**](string.md)| Input string as post body | [optional] 

### Return type

[**OuterString**](OuterString.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="testclientmodel"></a>
# **TestClientModel**
> Client TestClientModel (Client body)

To test \"client\" model

To test \"client\" model

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class TestClientModelExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var body = new Client(); // Client | client model

            try
            {
                // To test \"client\" model
                Client result = apiInstance.TestClientModel(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestClientModel: " + e.Message );
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
<a name="testendpointparameters"></a>
# **TestEndpointParameters**
> void TestEndpointParameters (Body2 body)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class TestEndpointParametersExample
    {
        public void main()
        {
            // Configure HTTP basic authorization: http_basic_test
            Configuration.Default.Username = "YOUR_USERNAME";
            Configuration.Default.Password = "YOUR_PASSWORD";

            var apiInstance = new FakeApi();
            var body = new Body2(); // Body2 | 

            try
            {
                // Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
                apiInstance.TestEndpointParameters(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestEndpointParameters: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Body2**](Body2.md)|  | 

### Return type

void (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="testenumparameters"></a>
# **TestEnumParameters**
> void TestEnumParameters (List<string> enumHeaderStringArray = null, string enumHeaderString = null, List<string> enumQueryStringArray = null, string enumQueryString = null, int? enumQueryInteger = null)

To test enum parameters

To test enum parameters

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class TestEnumParametersExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var enumHeaderStringArray = new List<string>(); // List<string> | Header parameter enum test (string array) (optional) 
            var enumHeaderString = enumHeaderString_example;  // string | Header parameter enum test (string) (optional)  (default to -efg)
            var enumQueryStringArray = new List<string>(); // List<string> | Query parameter enum test (string array) (optional) 
            var enumQueryString = enumQueryString_example;  // string | Query parameter enum test (string) (optional)  (default to -efg)
            var enumQueryInteger = 56;  // int? | Query parameter enum test (double) (optional) 

            try
            {
                // To test enum parameters
                apiInstance.TestEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestEnumParameters: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**List&lt;string&gt;**](string.md)| Header parameter enum test (string array) | [optional] 
 **enumHeaderString** | **string**| Header parameter enum test (string) | [optional] [default to -efg]
 **enumQueryStringArray** | [**List&lt;string&gt;**](string.md)| Query parameter enum test (string array) | [optional] 
 **enumQueryString** | **string**| Query parameter enum test (string) | [optional] [default to -efg]
 **enumQueryInteger** | **int?**| Query parameter enum test (double) | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="testenumrequestbody"></a>
# **TestEnumRequestBody**
> void TestEnumRequestBody (Body4 body = null)

To test enum parameters

To test enum parameters

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class TestEnumRequestBodyExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var body = new Body4(); // Body4 |  (optional) 

            try
            {
                // To test enum parameters
                apiInstance.TestEnumRequestBody(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestEnumRequestBody: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Body4**](Body4.md)|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="testinlineadditionalproperties"></a>
# **TestInlineAdditionalProperties**
> void TestInlineAdditionalProperties (Dictionary<string, string> body)

test inline additionalProperties

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class TestInlineAdditionalPropertiesExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var body = new Dictionary<string, string>(); // Dictionary<string, string> | request body

            try
            {
                // test inline additionalProperties
                apiInstance.TestInlineAdditionalProperties(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestInlineAdditionalProperties: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Dictionary&lt;string, string&gt;**](Dictionary&lt;string, string&gt;.md)| request body | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
<a name="testjsonformdata"></a>
# **TestJsonFormData**
> void TestJsonFormData (Body5 body)

test json serialization of form data

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class TestJsonFormDataExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var body = new Body5(); // Body5 | 

            try
            {
                // test json serialization of form data
                apiInstance.TestJsonFormData(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestJsonFormData: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Body5**](Body5.md)|  | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)
