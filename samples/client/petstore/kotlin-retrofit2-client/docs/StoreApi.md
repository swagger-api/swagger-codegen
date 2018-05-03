# StoreApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteOrder**](StoreApi.md#deleteOrder) | **DELETE** store/order/{order_id} | Delete purchase order by ID
[**getInventory**](StoreApi.md#getInventory) | **GET** store/inventory | Returns pet inventories by status
[**getOrderById**](StoreApi.md#getOrderById) | **GET** store/order/{order_id} | Find purchase order by ID
[**placeOrder**](StoreApi.md#placeOrder) | **POST** store/order | Place an order for a pet


<a name="deleteOrder"></a>
# **deleteOrder**
> deleteOrder(orderId)

Delete purchase order by ID

For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```kotlin
// Import classes:
//import io.swagger.client.infrastructure.*
//import io.swagger.client.models.*

val apiInstance = ApiClient().createService(StoreApi::class.java)
val orderId : kotlin.String = orderId_example // kotlin.String | ID of the order that needs to be deleted
val result = apiInstance.deleteOrder(orderId).execute()
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **kotlin.String**| ID of the order that needs to be deleted |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="getInventory"></a>
# **getInventory**
> kotlin.collections.Map&lt;kotlin.String, kotlin.Int&gt; getInventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```kotlin
// Import classes:
//import io.swagger.client.infrastructure.*
//import io.swagger.client.models.*

val apiInstance = ApiClient().createService(StoreApi::class.java)
val result = apiInstance.getInventory().execute()
println(result.body())
```

### Parameters
This endpoint does not need any parameter.

### Return type

**kotlin.collections.Map&lt;kotlin.String, kotlin.Int&gt;**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

<a name="getOrderById"></a>
# **getOrderById**
> Order getOrderById(orderId)

Find purchase order by ID

For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions

### Example
```kotlin
// Import classes:
//import io.swagger.client.infrastructure.*
//import io.swagger.client.models.*

val apiInstance = ApiClient().createService(StoreApi::class.java)
val orderId : kotlin.Long = 789 // kotlin.Long | ID of pet that needs to be fetched
val result = apiInstance.getOrderById(orderId).execute()
println(result.body())
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **kotlin.Long**| ID of pet that needs to be fetched |

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="placeOrder"></a>
# **placeOrder**
> Order placeOrder(body)

Place an order for a pet



### Example
```kotlin
// Import classes:
//import io.swagger.client.infrastructure.*
//import io.swagger.client.models.*

val apiInstance = ApiClient().createService(StoreApi::class.java)
val body : Order =  // Order | order placed for purchasing the pet
val result = apiInstance.placeOrder(body).execute()
println(result.body())
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Order**](Order.md)| order placed for purchasing the pet |

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

