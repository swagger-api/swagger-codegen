# SwaggerPetstore.Fake_classname_tags123Api

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClassname**](Fake_classname_tags123Api.md#testClassname) | **PATCH** /fake_classname_test | To test class name in snake case
[**testClassname_0**](Fake_classname_tags123Api.md#testClassname_0) | **PATCH** /fake_classname_test_optional | To test class name in snake case


<a name="testClassname"></a>
# **testClassname**
> Client testClassname(body)

To test class name in snake case

### Example
```javascript
var SwaggerPetstore = require('swagger_petstore');

var apiInstance = new SwaggerPetstore.Fake_classname_tags123Api();

var body = new SwaggerPetstore.Client(); // Client | client model

apiInstance.testClassname(body).then(function(data) {
  console.log('API called successfully. Returned data: ' + data);
}, function(error) {
  console.error(error);
});

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

<a name="testClassname_0"></a>
# **testClassname_0**
> Client testClassname_0(opts)

To test class name in snake case

### Example
```javascript
var SwaggerPetstore = require('swagger_petstore');

var apiInstance = new SwaggerPetstore.Fake_classname_tags123Api();

var opts = { 
  'body': new SwaggerPetstore.Client() // Client | client model
};
apiInstance.testClassname_0(opts).then(function(data) {
  console.log('API called successfully. Returned data: ' + data);
}, function(error) {
  console.error(error);
});

```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md)| client model | [optional] 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

