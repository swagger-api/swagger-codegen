# WWW::SwaggerClient::CatalogApi

## Load the API package
```perl
use WWW::SwaggerClient::Object::CatalogApi;
```

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**definition1**](CatalogApi.md#definition1) | **GET** /api/fakedef1 | Definition 1
[**definition2**](CatalogApi.md#definition2) | **GET** /api/fakedef2 | Definition 2
[**definition3**](CatalogApi.md#definition3) | **GET** /api/fakedef3 | Definition 3


# **definition1**
> ARRAY[Definition1] definition1()

Definition 1

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::CatalogApi;
my $api_instance = WWW::SwaggerClient::CatalogApi->new(
);


eval { 
    my $result = $api_instance->definition1();
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling CatalogApi->definition1: $@\n";
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**ARRAY[Definition1]**](Definition1.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, text/json, application/xml, text/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **definition2**
> ARRAY[Definition2] definition2()

Definition 2

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::CatalogApi;
my $api_instance = WWW::SwaggerClient::CatalogApi->new(
);


eval { 
    my $result = $api_instance->definition2();
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling CatalogApi->definition2: $@\n";
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**ARRAY[Definition2]**](Definition2.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, text/json, application/xml, text/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **definition3**
> ARRAY[Definition3] definition3()

Definition 3

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::CatalogApi;
my $api_instance = WWW::SwaggerClient::CatalogApi->new(
);


eval { 
    my $result = $api_instance->definition3();
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling CatalogApi->definition3: $@\n";
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**ARRAY[Definition3]**](Definition3.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, text/json, application/xml, text/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

