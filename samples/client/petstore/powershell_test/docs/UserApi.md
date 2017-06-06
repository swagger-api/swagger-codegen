# IO.Swagger.IO.Swagger/API.UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createUser**](UserApi.md#createuser) | **POST** /user | Create user
[**createUsersWithArrayInput**](UserApi.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array
[**createUsersWithListInput**](UserApi.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array
[**deleteUser**](UserApi.md#deleteuser) | **DELETE** /user/{username} | Delete user
[**getUserByName**](UserApi.md#getuserbyname) | **GET** /user/{username} | Get user by user name
[**loginUser**](UserApi.md#loginuser) | **GET** /user/login | Logs user into the system
[**logoutUser**](UserApi.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session
[**updateUser**](UserApi.md#updateuser) | **PUT** /user/{username} | Updated user


<a name="createuser"></a>
# **createUser**
> void createUser (User body)

Create user

This can only be done by the logged in user.

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class createUserExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var body = new User(); // User | Created user object

            try
            {
                // Create user
                apiInstance.createUser(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.createUser: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**User**](User.md)| Created user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="createuserswitharrayinput"></a>
# **createUsersWithArrayInput**
> void createUsersWithArrayInput ([User] body)

Creates list of users with given input array



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class createUsersWithArrayInputExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var body = new [User](); // [User] | List of user object

            try
            {
                // Creates list of users with given input array
                apiInstance.createUsersWithArrayInput(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.createUsersWithArrayInput: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**[User]**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="createuserswithlistinput"></a>
# **createUsersWithListInput**
> void createUsersWithListInput ([User] body)

Creates list of users with given input array



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class createUsersWithListInputExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var body = new [User](); // [User] | List of user object

            try
            {
                // Creates list of users with given input array
                apiInstance.createUsersWithListInput(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.createUsersWithListInput: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**[User]**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="deleteuser"></a>
# **deleteUser**
> void deleteUser (String username)

Delete user

This can only be done by the logged in user.

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class deleteUserExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var username = username_example;  // String | The name that needs to be deleted

            try
            {
                // Delete user
                apiInstance.deleteUser(username);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.deleteUser: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The name that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="getuserbyname"></a>
# **getUserByName**
> User getUserByName (String username)

Get user by user name



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class getUserByNameExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var username = username_example;  // String | The name that needs to be fetched. Use user1 for testing. 

            try
            {
                // Get user by user name
                User result = apiInstance.getUserByName(username);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.getUserByName: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The name that needs to be fetched. Use user1 for testing.  | 

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="loginuser"></a>
# **loginUser**
> String loginUser (String username, String password)

Logs user into the system



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class loginUserExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var username = username_example;  // String | The user name for login
            var password = password_example;  // String | The password for login in clear text

            try
            {
                // Logs user into the system
                String result = apiInstance.loginUser(username, password);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.loginUser: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The user name for login | 
 **password** | **String**| The password for login in clear text | 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="logoutuser"></a>
# **logoutUser**
> void logoutUser ()

Logs out current logged in user session



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class logoutUserExample
    {
        public void main()
        {
            var apiInstance = new UserApi();

            try
            {
                // Logs out current logged in user session
                apiInstance.logoutUser();
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.logoutUser: " + e.Message );
            }
        }
    }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="updateuser"></a>
# **updateUser**
> void updateUser (String username, User body)

Updated user

This can only be done by the logged in user.

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger/API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger/Model;

namespace Example
{
    public class updateUserExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var username = username_example;  // String | name that need to be deleted
            var body = new User(); // User | Updated user object

            try
            {
                // Updated user
                apiInstance.updateUser(username, body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.updateUser: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| name that need to be deleted | 
 **body** | [**User**](User.md)| Updated user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

