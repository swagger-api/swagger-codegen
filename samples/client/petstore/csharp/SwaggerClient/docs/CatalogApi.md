# IO.Swagger.Api.CatalogApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Definition1**](CatalogApi.md#definition1) | **GET** /api/fakedef1 | Definition 1
[**Definition2**](CatalogApi.md#definition2) | **GET** /api/fakedef2 | Definition 2
[**Definition3**](CatalogApi.md#definition3) | **GET** /api/fakedef3 | Definition 3


<a name="definition1"></a>
# **Definition1**
> List<Definition1> Definition1 ()

Definition 1

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class Definition1Example
    {
        public void main()
        {
            
            var apiInstance = new CatalogApi();

            try
            {
                // Definition 1
                List&lt;Definition1&gt; result = apiInstance.Definition1();
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling CatalogApi.Definition1: " + e.Message );
            }
        }
    }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**List<Definition1>**](Definition1.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, text/json, application/xml, text/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="definition2"></a>
# **Definition2**
> List<Definition2> Definition2 ()

Definition 2

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class Definition2Example
    {
        public void main()
        {
            
            var apiInstance = new CatalogApi();

            try
            {
                // Definition 2
                List&lt;Definition2&gt; result = apiInstance.Definition2();
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling CatalogApi.Definition2: " + e.Message );
            }
        }
    }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**List<Definition2>**](Definition2.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, text/json, application/xml, text/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="definition3"></a>
# **Definition3**
> List<Definition3> Definition3 ()

Definition 3

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class Definition3Example
    {
        public void main()
        {
            
            var apiInstance = new CatalogApi();

            try
            {
                // Definition 3
                List&lt;Definition3&gt; result = apiInstance.Definition3();
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling CatalogApi.Definition3: " + e.Message );
            }
        }
    }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**List<Definition3>**](Definition3.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, text/json, application/xml, text/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

