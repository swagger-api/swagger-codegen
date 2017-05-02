# Swagger\Client\CatalogApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**definition1**](CatalogApi.md#definition1) | **GET** /api/fakedef1 | Definition 1
[**definition2**](CatalogApi.md#definition2) | **GET** /api/fakedef2 | Definition 2
[**definition3**](CatalogApi.md#definition3) | **GET** /api/fakedef3 | Definition 3


# **definition1**
> \Swagger\Client\Model\Definition1[] definition1()

Definition 1

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\CatalogApi(new \Http\Adapter\Guzzle6\Client());

try {
    $result = $api_instance->definition1();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling CatalogApi->definition1: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**\Swagger\Client\Model\Definition1[]**](../Model/Definition1.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, text/json, application/xml, text/xml

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **definition2**
> \Swagger\Client\Model\Definition2[] definition2()

Definition 2

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\CatalogApi(new \Http\Adapter\Guzzle6\Client());

try {
    $result = $api_instance->definition2();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling CatalogApi->definition2: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**\Swagger\Client\Model\Definition2[]**](../Model/Definition2.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, text/json, application/xml, text/xml

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **definition3**
> \Swagger\Client\Model\Definition3[] definition3()

Definition 3

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\CatalogApi(new \Http\Adapter\Guzzle6\Client());

try {
    $result = $api_instance->definition3();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling CatalogApi->definition3: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**\Swagger\Client\Model\Definition3[]**](../Model/Definition3.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, text/json, application/xml, text/xml

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

