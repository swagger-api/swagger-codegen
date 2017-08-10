# \user_api

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**create_user**](user_api.md#create_user) | **POST** /user | Create user
[**create_users_with_array_input**](user_api.md#create_users_with_array_input) | **POST** /user/createWithArray | Creates list of users with given input array
[**create_users_with_list_input**](user_api.md#create_users_with_list_input) | **POST** /user/createWithList | Creates list of users with given input array
[**delete_user**](user_api.md#delete_user) | **DELETE** /user/{username} | Delete user
[**get_user_by_name**](user_api.md#get_user_by_name) | **GET** /user/{username} | Get user by user name
[**login_user**](user_api.md#login_user) | **GET** /user/login | Logs user into the system
[**logout_user**](user_api.md#logout_user) | **GET** /user/logout | Logs out current logged in user session
[**update_user**](user_api.md#update_user) | **PUT** /user/{username} | Updated user


# **create_user**
> create_user(body)
Create user

This can only be done by the logged in user.

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **body** | [**user**](user.md)| Created user object | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_users_with_array_input**
> create_users_with_array_input(body)
Creates list of users with given input array



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **body** | [**{}**](User.md)| List of user object | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_users_with_list_input**
> create_users_with_list_input(body)
Creates list of users with given input array



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **body** | [**{}**](User.md)| List of user object | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_user**
> delete_user(username)
Delete user

This can only be done by the logged in user.

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **username** | **string**| The name that needs to be deleted | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_user_by_name**
> user get_user_by_name(username)
Get user by user name



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **username** | **string**| The name that needs to be fetched. Use user1 for testing.  | 

### Return type

[**user**](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **login_user**
> string login_user(username, password)
Logs user into the system



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **username** | **string**| The user name for login | 
  **password** | **string**| The password for login in clear text | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **logout_user**
> logout_user()
Logs out current logged in user session



### Required Parameters
This endpoint does not need any parameter.

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_user**
> update_user(username, body)
Updated user

This can only be done by the logged in user.

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **username** | **string**| name that need to be deleted | 
  **body** | [**user**](user.md)| Updated user object | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

