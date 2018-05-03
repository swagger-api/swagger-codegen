# io.swagger.client - Kotlin client library for Swagger Petstore

## Requires

* Kotlin 1.1.2
* Gradle 3.3

## Build

First, create the gradle wrapper script:

```
gradle wrapper
```

Then, run:

```
./gradlew check assemble
```

This runs all tests and packages the library.

## Features/Implementation Notes

* Supports JSON inputs/outputs, File inputs, and Form inputs.
* Supports collection formats for query parameters: csv, tsv, ssv, pipes.
* Some Kotlin and Java types are fully qualified to avoid conflicts with types defined in Swagger definitions.
* Implementation of ApiClient is intended to reduce method counts, specifically to benefit Android targets.

<a name="documentation-for-api-endpoints"></a>
## Documentation for API Endpoints

All URIs are relative to *http://petstore.swagger.io:80/v2*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*AnotherFakeApi* | [**testSpecialTags**](docs/AnotherFakeApi.md#testspecialtags) | **PATCH** another-fake/dummy | To test special tags
*FakeApi* | [**fakeOuterBooleanSerialize**](docs/FakeApi.md#fakeouterbooleanserialize) | **POST** fake/outer/boolean | 
*FakeApi* | [**fakeOuterCompositeSerialize**](docs/FakeApi.md#fakeoutercompositeserialize) | **POST** fake/outer/composite | 
*FakeApi* | [**fakeOuterNumberSerialize**](docs/FakeApi.md#fakeouternumberserialize) | **POST** fake/outer/number | 
*FakeApi* | [**fakeOuterStringSerialize**](docs/FakeApi.md#fakeouterstringserialize) | **POST** fake/outer/string | 
*FakeApi* | [**testBodyWithQueryParams**](docs/FakeApi.md#testbodywithqueryparams) | **PUT** fake/body-with-query-params | 
*FakeApi* | [**testClientModel**](docs/FakeApi.md#testclientmodel) | **PATCH** fake | To test \"client\" model
*FakeApi* | [**testEndpointParameters**](docs/FakeApi.md#testendpointparameters) | **POST** fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
*FakeApi* | [**testEnumParameters**](docs/FakeApi.md#testenumparameters) | **GET** fake | To test enum parameters
*FakeApi* | [**testInlineAdditionalProperties**](docs/FakeApi.md#testinlineadditionalproperties) | **POST** fake/inline-additionalProperties | test inline additionalProperties
*FakeApi* | [**testJsonFormData**](docs/FakeApi.md#testjsonformdata) | **GET** fake/jsonFormData | test json serialization of form data
*FakeClassnameTags123Api* | [**testClassname**](docs/FakeClassnameTags123Api.md#testclassname) | **PATCH** fake_classname_test | To test class name in snake case
*PetApi* | [**addPet**](docs/PetApi.md#addpet) | **POST** pet | Add a new pet to the store
*PetApi* | [**deletePet**](docs/PetApi.md#deletepet) | **DELETE** pet/{petId} | Deletes a pet
*PetApi* | [**findPetsByStatus**](docs/PetApi.md#findpetsbystatus) | **GET** pet/findByStatus | Finds Pets by status
*PetApi* | [**findPetsByTags**](docs/PetApi.md#findpetsbytags) | **GET** pet/findByTags | Finds Pets by tags
*PetApi* | [**getPetById**](docs/PetApi.md#getpetbyid) | **GET** pet/{petId} | Find pet by ID
*PetApi* | [**updatePet**](docs/PetApi.md#updatepet) | **PUT** pet | Update an existing pet
*PetApi* | [**updatePetWithForm**](docs/PetApi.md#updatepetwithform) | **POST** pet/{petId} | Updates a pet in the store with form data
*PetApi* | [**uploadFile**](docs/PetApi.md#uploadfile) | **POST** pet/{petId}/uploadImage | uploads an image
*StoreApi* | [**deleteOrder**](docs/StoreApi.md#deleteorder) | **DELETE** store/order/{order_id} | Delete purchase order by ID
*StoreApi* | [**getInventory**](docs/StoreApi.md#getinventory) | **GET** store/inventory | Returns pet inventories by status
*StoreApi* | [**getOrderById**](docs/StoreApi.md#getorderbyid) | **GET** store/order/{order_id} | Find purchase order by ID
*StoreApi* | [**placeOrder**](docs/StoreApi.md#placeorder) | **POST** store/order | Place an order for a pet
*UserApi* | [**createUser**](docs/UserApi.md#createuser) | **POST** user | Create user
*UserApi* | [**createUsersWithArrayInput**](docs/UserApi.md#createuserswitharrayinput) | **POST** user/createWithArray | Creates list of users with given input array
*UserApi* | [**createUsersWithListInput**](docs/UserApi.md#createuserswithlistinput) | **POST** user/createWithList | Creates list of users with given input array
*UserApi* | [**deleteUser**](docs/UserApi.md#deleteuser) | **DELETE** user/{username} | Delete user
*UserApi* | [**getUserByName**](docs/UserApi.md#getuserbyname) | **GET** user/{username} | Get user by user name
*UserApi* | [**loginUser**](docs/UserApi.md#loginuser) | **GET** user/login | Logs user into the system
*UserApi* | [**logoutUser**](docs/UserApi.md#logoutuser) | **GET** user/logout | Logs out current logged in user session
*UserApi* | [**updateUser**](docs/UserApi.md#updateuser) | **PUT** user/{username} | Updated user


<a name="documentation-for-models"></a>
## Documentation for Models

 - [io.swagger.client.models.AdditionalPropertiesClass](docs/AdditionalPropertiesClass.md)
 - [io.swagger.client.models.Animal](docs/Animal.md)
 - [io.swagger.client.models.AnimalFarm](docs/AnimalFarm.md)
 - [io.swagger.client.models.ApiResponse](docs/ApiResponse.md)
 - [io.swagger.client.models.ArrayOfArrayOfNumberOnly](docs/ArrayOfArrayOfNumberOnly.md)
 - [io.swagger.client.models.ArrayOfNumberOnly](docs/ArrayOfNumberOnly.md)
 - [io.swagger.client.models.ArrayTest](docs/ArrayTest.md)
 - [io.swagger.client.models.Capitalization](docs/Capitalization.md)
 - [io.swagger.client.models.Category](docs/Category.md)
 - [io.swagger.client.models.ClassModel](docs/ClassModel.md)
 - [io.swagger.client.models.Client](docs/Client.md)
 - [io.swagger.client.models.DollarspecialLeftSquareBracketmodelnameRightSquareBracket](docs/DollarspecialLeftSquareBracketmodelnameRightSquareBracket.md)
 - [io.swagger.client.models.EnumArrays](docs/EnumArrays.md)
 - [io.swagger.client.models.EnumClass](docs/EnumClass.md)
 - [io.swagger.client.models.EnumTest](docs/EnumTest.md)
 - [io.swagger.client.models.FormatTest](docs/FormatTest.md)
 - [io.swagger.client.models.HasOnlyReadOnly](docs/HasOnlyReadOnly.md)
 - [io.swagger.client.models.List](docs/List.md)
 - [io.swagger.client.models.MapTest](docs/MapTest.md)
 - [io.swagger.client.models.MixedPropertiesAndAdditionalPropertiesClass](docs/MixedPropertiesAndAdditionalPropertiesClass.md)
 - [io.swagger.client.models.Model200Response](docs/Model200Response.md)
 - [io.swagger.client.models.Name](docs/Name.md)
 - [io.swagger.client.models.NumberOnly](docs/NumberOnly.md)
 - [io.swagger.client.models.Order](docs/Order.md)
 - [io.swagger.client.models.OuterBoolean](docs/OuterBoolean.md)
 - [io.swagger.client.models.OuterComposite](docs/OuterComposite.md)
 - [io.swagger.client.models.OuterEnum](docs/OuterEnum.md)
 - [io.swagger.client.models.OuterNumber](docs/OuterNumber.md)
 - [io.swagger.client.models.OuterString](docs/OuterString.md)
 - [io.swagger.client.models.Pet](docs/Pet.md)
 - [io.swagger.client.models.ReadOnlyFirst](docs/ReadOnlyFirst.md)
 - [io.swagger.client.models.Return](docs/Return.md)
 - [io.swagger.client.models.Tag](docs/Tag.md)
 - [io.swagger.client.models.User](docs/User.md)
 - [io.swagger.client.models.Cat](docs/Cat.md)
 - [io.swagger.client.models.Dog](docs/Dog.md)


<a name="documentation-for-authorization"></a>
## Documentation for Authorization

<a name="api_key"></a>
### api_key

- **Type**: API key
- **API key parameter name**: api_key
- **Location**: HTTP header

<a name="api_key_query"></a>
### api_key_query

- **Type**: API key
- **API key parameter name**: api_key_query
- **Location**: URL query string

<a name="http_basic_test"></a>
### http_basic_test

- **Type**: HTTP basic authentication

<a name="petstore_auth"></a>
### petstore_auth

- **Type**: OAuth
- **Flow**: implicit
- **Authorization URL**: http://petstore.swagger.io/api/oauth/dialog
- **Scopes**: 
  - write:pets: modify pets in your account
  - read:pets: read your pets

