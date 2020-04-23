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

All URIs are relative to */*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*DefaultApi* | [**testMethod**](docs/DefaultApi.md#testmethod) | **GET** /test | 
*PetApi* | [**addPet**](docs/PetApi.md#addpet) | **POST** /pet | Add a new pet to the store
*PetApi* | [**deletePet**](docs/PetApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
*PetApi* | [**findPetsByStatus**](docs/PetApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
*PetApi* | [**findPetsByTags**](docs/PetApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
*PetApi* | [**getAllPets**](docs/PetApi.md#getallpets) | **GET** /allPets | 
*PetApi* | [**getPetById**](docs/PetApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
*PetApi* | [**getRandomPet**](docs/PetApi.md#getrandompet) | **GET** /randomPet | 
*PetApi* | [**updatePet**](docs/PetApi.md#updatepet) | **PUT** /pet | Update an existing pet
*PetApi* | [**updatePetWithForm**](docs/PetApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
*PetApi* | [**uploadFile**](docs/PetApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image
*StoreApi* | [**deleteOrder**](docs/StoreApi.md#deleteorder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
*StoreApi* | [**getInventory**](docs/StoreApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status
*StoreApi* | [**getOrderById**](docs/StoreApi.md#getorderbyid) | **GET** /store/order/{orderId} | Find purchase order by ID
*StoreApi* | [**placeOrder**](docs/StoreApi.md#placeorder) | **POST** /store/order | Place an order for a pet
*UserApi* | [**createUser**](docs/UserApi.md#createuser) | **POST** /user | Create user
*UserApi* | [**createUsersWithArrayInput**](docs/UserApi.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array
*UserApi* | [**createUsersWithListInput**](docs/UserApi.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array
*UserApi* | [**deleteUser**](docs/UserApi.md#deleteuser) | **DELETE** /user/{username} | Delete user
*UserApi* | [**getUserByName**](docs/UserApi.md#getuserbyname) | **GET** /user/{username} | Get user by user name
*UserApi* | [**loginUser**](docs/UserApi.md#loginuser) | **GET** /user/login | Logs user into the system
*UserApi* | [**logoutUser**](docs/UserApi.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session
*UserApi* | [**userUsernamePut**](docs/UserApi.md#userusernameput) | **PUT** /user/{username} | Updated user

<a name="documentation-for-models"></a>
## Documentation for Models

 - [io.swagger.client.models.AllPetsResponse](docs/AllPetsResponse.md)
 - [io.swagger.client.models.ApiResponse](docs/ApiResponse.md)
 - [io.swagger.client.models.Body](docs/Body.md)
 - [io.swagger.client.models.Cat](docs/Cat.md)
 - [io.swagger.client.models.Category](docs/Category.md)
 - [io.swagger.client.models.Dog](docs/Dog.md)
 - [io.swagger.client.models.NullableEnumModel](docs/NullableEnumModel.md)
 - [io.swagger.client.models.OneOfAllPetsResponseItems](docs/OneOfAllPetsResponseItems.md)
 - [io.swagger.client.models.OneOfSinglePetResponsePet](docs/OneOfSinglePetResponsePet.md)
 - [io.swagger.client.models.Order](docs/Order.md)
 - [io.swagger.client.models.Pet](docs/Pet.md)
 - [io.swagger.client.models.SinglePetResponse](docs/SinglePetResponse.md)
 - [io.swagger.client.models.Tag](docs/Tag.md)
 - [io.swagger.client.models.Test](docs/Test.md)
 - [io.swagger.client.models.User](docs/User.md)

<a name="documentation-for-authorization"></a>
## Documentation for Authorization

<a name="api_key"></a>
### api_key


<a name="bearer"></a>
### bearer


<a name="petstore_auth"></a>
### petstore_auth

- **Type**: OAuth
- **Flow**: implicit
- **Authorization URL**: http://petstore.swagger.io/oauth/dialog
- **Scopes**: 
  - : 

