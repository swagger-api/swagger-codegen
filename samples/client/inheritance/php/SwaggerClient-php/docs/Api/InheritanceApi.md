# Swagger\Client\InheritanceApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fooGet**](InheritanceApi.md#fooGet) | **GET** /foo | Foo


# **fooGet**
> \Swagger\Client\Model\InheritanceBase[] fooGet()

Foo

Foo

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\InheritanceApi();

try {
    $result = $api_instance->fooGet();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling InheritanceApi->fooGet: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**\Swagger\Client\Model\InheritanceBase[]**](../Model/InheritanceBase.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

