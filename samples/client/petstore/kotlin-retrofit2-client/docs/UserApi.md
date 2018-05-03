# UserApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createUser**](UserApi.md#createUser) | **POST** user | Create user
[**createUsersWithArrayInput**](UserApi.md#createUsersWithArrayInput) | **POST** user/createWithArray | Creates list of users with given input array
[**createUsersWithListInput**](UserApi.md#createUsersWithListInput) | **POST** user/createWithList | Creates list of users with given input array
[**deleteUser**](UserApi.md#deleteUser) | **DELETE** user/{username} | Delete user
[**getUserByName**](UserApi.md#getUserByName) | **GET** user/{username} | Get user by user name
[**loginUser**](UserApi.md#loginUser) | **GET** user/login | Logs user into the system
[**logoutUser**](UserApi.md#logoutUser) | **GET** user/logout | Logs out current logged in user session
[**updateUser**](UserApi.md#updateUser) | **PUT** user/{username} | Updated user


<a name="createUser"></a>
# **createUser**
> createUser(body)

Create user

This can only be done by the logged in user.

### Example
```kotlin
// Import classes:
//import io.swagger.client.infrastructure.*
//import io.swagger.client.models.*

val apiInstance = ApiClient().createService(UserApi::class.java)
val body : User =  // User | Created user object
val result = apiInstance.createUser(body).execute()
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**User**](User.md)| Created user object |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="createUsersWithArrayInput"></a>
# **createUsersWithArrayInput**
> createUsersWithArrayInput(body)

Creates list of users with given input array



### Example
```kotlin
// Import classes:
//import io.swagger.client.infrastructure.*
//import io.swagger.client.models.*

val apiInstance = ApiClient().createService(UserApi::class.java)
val body : kotlin.collections.List<User> =  // kotlin.collections.List<User> | List of user object
val result = apiInstance.createUsersWithArrayInput(body).execute()
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**kotlin.collections.List&lt;User&gt;**](User.md)| List of user object |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="createUsersWithListInput"></a>
# **createUsersWithListInput**
> createUsersWithListInput(body)

Creates list of users with given input array



### Example
```kotlin
// Import classes:
//import io.swagger.client.infrastructure.*
//import io.swagger.client.models.*

val apiInstance = ApiClient().createService(UserApi::class.java)
val body : kotlin.collections.List<User> =  // kotlin.collections.List<User> | List of user object
val result = apiInstance.createUsersWithListInput(body).execute()
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**kotlin.collections.List&lt;User&gt;**](User.md)| List of user object |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="deleteUser"></a>
# **deleteUser**
> deleteUser(username)

Delete user

This can only be done by the logged in user.

### Example
```kotlin
// Import classes:
//import io.swagger.client.infrastructure.*
//import io.swagger.client.models.*

val apiInstance = ApiClient().createService(UserApi::class.java)
val username : kotlin.String = username_example // kotlin.String | The name that needs to be deleted
val result = apiInstance.deleteUser(username).execute()
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **kotlin.String**| The name that needs to be deleted |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="getUserByName"></a>
# **getUserByName**
> User getUserByName(username)

Get user by user name



### Example
```kotlin
// Import classes:
//import io.swagger.client.infrastructure.*
//import io.swagger.client.models.*

val apiInstance = ApiClient().createService(UserApi::class.java)
val username : kotlin.String = username_example // kotlin.String | The name that needs to be fetched. Use user1 for testing.
val result = apiInstance.getUserByName(username).execute()
println(result.body())
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **kotlin.String**| The name that needs to be fetched. Use user1 for testing. |

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="loginUser"></a>
# **loginUser**
> kotlin.String loginUser(username, password)

Logs user into the system



### Example
```kotlin
// Import classes:
//import io.swagger.client.infrastructure.*
//import io.swagger.client.models.*

val apiInstance = ApiClient().createService(UserApi::class.java)
val username : kotlin.String = username_example // kotlin.String | The user name for login
val password : kotlin.String = password_example // kotlin.String | The password for login in clear text
val result = apiInstance.loginUser(username, password).execute()
println(result.body())
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **kotlin.String**| The user name for login |
 **password** | **kotlin.String**| The password for login in clear text |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="logoutUser"></a>
# **logoutUser**
> logoutUser()

Logs out current logged in user session



### Example
```kotlin
// Import classes:
//import io.swagger.client.infrastructure.*
//import io.swagger.client.models.*

val apiInstance = ApiClient().createService(UserApi::class.java)
val result = apiInstance.logoutUser().execute()
```

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="updateUser"></a>
# **updateUser**
> updateUser(username, body)

Updated user

This can only be done by the logged in user.

### Example
```kotlin
// Import classes:
//import io.swagger.client.infrastructure.*
//import io.swagger.client.models.*

val apiInstance = ApiClient().createService(UserApi::class.java)
val username : kotlin.String = username_example // kotlin.String | name that need to be deleted
val body : User =  // User | Updated user object
val result = apiInstance.updateUser(username, body).execute()
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **kotlin.String**| name that need to be deleted |
 **body** | [**User**](User.md)| Updated user object |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

