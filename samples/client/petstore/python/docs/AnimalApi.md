# swagger_client.AnimalApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**add_animal**](AnimalApi.md#add_animal) | **POST** /animal | Add a new animal to the store
[**delete_animal**](AnimalApi.md#delete_animal) | **DELETE** /animal/{animalId} | Deletes a animal
[**get_animal_by_id**](AnimalApi.md#get_animal_by_id) | **GET** /animal/{animalId} | Find animal by ID
[**update_animal**](AnimalApi.md#update_animal) | **PUT** /animal | Update an existing animal
[**update_animal_with_form**](AnimalApi.md#update_animal_with_form) | **POST** /animal/{animalId} | Updates a animal

# **add_animal**
> add_animal(body)

Add a new animal to the store

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = swagger_client.AnimalApi()
body = swagger_client.Animal() # Animal | Animal object that needs to be added to the store

try:
    # Add a new animal to the store
    api_instance.add_animal(body)
except ApiException as e:
    print("Exception when calling AnimalApi->add_animal: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Animal**](Animal.md)| Animal object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_animal**
> delete_animal(animal_id, api_key=api_key)

Deletes a animal

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = swagger_client.AnimalApi()
animal_id = 789 # int | Animal id to delete
api_key = 'api_key_example' # str |  (optional)

try:
    # Deletes a animal
    api_instance.delete_animal(animal_id, api_key=api_key)
except ApiException as e:
    print("Exception when calling AnimalApi->delete_animal: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **animal_id** | **int**| Animal id to delete | 
 **api_key** | **str**|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_animal_by_id**
> Animal get_animal_by_id(animal_id)

Find animal by ID

Returns a single animal

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = swagger_client.AnimalApi()
animal_id = 789 # int | ID of pet to return

try:
    # Find animal by ID
    api_response = api_instance.get_animal_by_id(animal_id)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling AnimalApi->get_animal_by_id: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **animal_id** | **int**| ID of pet to return | 

### Return type

[**Animal**](Animal.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_animal**
> update_animal(body)

Update an existing animal

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = swagger_client.AnimalApi()
body = swagger_client.Animal() # Animal | Animal object that needs to be added.

try:
    # Update an existing animal
    api_instance.update_animal(body)
except ApiException as e:
    print("Exception when calling AnimalApi->update_animal: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Animal**](Animal.md)| Animal object that needs to be added. | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_animal_with_form**
> update_animal_with_form(animal_id, name=name, status=status)

Updates a animal

### Example
```python
from __future__ import print_function
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = swagger_client.AnimalApi()
animal_id = 789 # int | ID of animal that needs to be updated
name = 'name_example' # str |  (optional)
status = 'status_example' # str |  (optional)

try:
    # Updates a animal
    api_instance.update_animal_with_form(animal_id, name=name, status=status)
except ApiException as e:
    print("Exception when calling AnimalApi->update_animal_with_form: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **animal_id** | **int**| ID of animal that needs to be updated | 
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

