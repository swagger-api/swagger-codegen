# DefaultApi

All URIs are relative to *http://eastus.cloudapp.azure.com/alip-agents*

Method | HTTP request | Description
------------- | ------------- | -------------
[**agentsIdSummaryGet**](DefaultApi.md#agentsIdSummaryGet) | **GET** /agents/{id}/summary | 


<a name="agentsIdSummaryGet"></a>
# **agentsIdSummaryGet**
> Map&lt;String, Object&gt; agentsIdSummaryGet(clientId, clientSecret, id)



Get Agent Summary from ALIP

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.DefaultApi;


DefaultApi apiInstance = new DefaultApi();
String clientId = "clientId_example"; // String | 
String clientSecret = "clientSecret_example"; // String | 
String id = "id_example"; // String | Agent Number
try {
    Map<String, Object> result = apiInstance.agentsIdSummaryGet(clientId, clientSecret, id);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling DefaultApi#agentsIdSummaryGet");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **clientId** | **String**|  |
 **clientSecret** | **String**|  |
 **id** | **String**| Agent Number |

### Return type

**Map&lt;String, Object&gt;**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

