# swagger_client.DogApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**add_dog**](DogApi.md#add_dog) | **POST** /dog | Add a new dog to the store
[**delete_dog**](DogApi.md#delete_dog) | **DELETE** /dog/{dogId} | Deletes a dog
[**get_dog_by_id**](DogApi.md#get_dog_by_id) | **GET** /dog/{dogId} | Find dog by ID
[**update_dog**](DogApi.md#update_dog) | **PUT** /dog | Update an existing dog
[**update_dog_with_form**](DogApi.md#update_dog_with_form) | **POST** /dog/{dogId} | Updates a dog

# **add_dog**
> add_dog(body)

Add a new dog to the store

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = swagger_client.DogApi()
body = swagger_client.Dog() # Dog | Dog object that needs to be added to the store

try:
    # Add a new dog to the store
    api_instance.add_dog(body)
except ApiException as e:
    print("Exception when calling DogApi->add_dog: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Dog**](Dog.md)| Dog object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_dog**
> delete_dog(dog_id, api_key=api_key)

Deletes a dog

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = swagger_client.DogApi()
dog_id = 789 # int | Dog id to delete
api_key = 'api_key_example' # str |  (optional)

try:
    # Deletes a dog
    api_instance.delete_dog(dog_id, api_key=api_key)
except ApiException as e:
    print("Exception when calling DogApi->delete_dog: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **dog_id** | **int**| Dog id to delete | 
 **api_key** | **str**|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_dog_by_id**
> Dog get_dog_by_id(dog_id)

Find dog by ID

Returns a single dog

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = swagger_client.DogApi()
dog_id = 789 # int | ID of dog to return

try:
    # Find dog by ID
    api_response = api_instance.get_dog_by_id(dog_id)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling DogApi->get_dog_by_id: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **dog_id** | **int**| ID of dog to return | 

### Return type

[**Dog**](Dog.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_dog**
> update_dog(body)

Update an existing dog

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = swagger_client.DogApi()
body = swagger_client.Dog() # Dog | Dog object that needs to be added.

try:
    # Update an existing dog
    api_instance.update_dog(body)
except ApiException as e:
    print("Exception when calling DogApi->update_dog: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Dog**](Dog.md)| Dog object that needs to be added. | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_dog_with_form**
> update_dog_with_form(dog_id, name=name, status=status)

Updates a dog

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = swagger_client.DogApi()
dog_id = 789 # int | ID of dog that needs to be updated
name = 'name_example' # str |  (optional)
status = 'status_example' # str |  (optional)

try:
    # Updates a dog
    api_instance.update_dog_with_form(dog_id, name=name, status=status)
except ApiException as e:
    print("Exception when calling DogApi->update_dog_with_form: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **dog_id** | **int**| ID of dog that needs to be updated | 
 **name** | **str**|  | [optional] 
 **status** | **str**|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

