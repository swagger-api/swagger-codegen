# swagger_client.FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_classname**](FakeClassnameTags123Api.md#test_classname) | **PATCH** /fake_classname_test | To test class name in snake case

# **test_classname**
> Client test_classname(body)

To test class name in snake case

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# Configure API key authorization: api_key_query
configuration = swagger_client.Configuration()
configuration.api_key['api_key_query'] = 'YOUR_API_KEY'
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
# configuration.api_key_prefix['api_key_query'] = 'Bearer'

# create an instance of the API class
api_instance = swagger_client.FakeClassnameTags123Api(swagger_client.ApiClient(configuration))
body = swagger_client.Client() # Client | client model

try:
    # To test class name in snake case
    api_response = api_instance.test_classname(body)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling FakeClassnameTags123Api->test_classname: %s\n" % e)
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

