package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import io.swagger.client.model.Body1;
import io.swagger.client.model.Body2;
import io.swagger.client.model.InlineResponse200;
import io.swagger.client.model.InlineResponse2001;
import io.swagger.client.model.ModelApiResponse;
import io.swagger.client.model.Pet;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface PetApi {
  /**
   * Add a new parrow to the store
   * Sync method
   * 
   * @param body  (optional)
   * @return InlineResponse2001
   */
  @POST("/parrot")
  InlineResponse2001 addParrot(
    @retrofit.http.Body Body2 body
  );

  /**
   * Add a new parrow to the store
   * Async method
   * @param body  (optional)
   * @param cb callback method
   */
  @POST("/parrot")
  void addParrot(
    @retrofit.http.Body Body2 body, Callback<InlineResponse2001> cb
  );
  /**
   * Add a new pet to the store
   * Sync method
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @return Void
   */
  @POST("/pet")
  Void addPet(
    @retrofit.http.Body Pet body
  );

  /**
   * Add a new pet to the store
   * Async method
   * @param body Pet object that needs to be added to the store (required)
   * @param cb callback method
   */
  @POST("/pet")
  void addPet(
    @retrofit.http.Body Pet body, Callback<Void> cb
  );
  /**
   * Deletes a pet
   * Sync method
   * 
   * @param petId Pet id to delete (required)
   * @param apiKey  (optional)
   * @return Void
   */
  @DELETE("/pet/{petId}")
  Void deletePet(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Header("api_key") String apiKey
  );

  /**
   * Deletes a pet
   * Async method
   * @param petId Pet id to delete (required)
   * @param apiKey  (optional)
   * @param cb callback method
   */
  @DELETE("/pet/{petId}")
  void deletePet(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Header("api_key") String apiKey, Callback<Void> cb
  );
  /**
   * Find pet by ID
   * Sync method
   * schedule pet feeding
   * @param body Pet object that needs to be added to the store (required)
   * @param token status (required)
   * @param petType type of food (required)
   * @param status status (required)
   * @param petId ID of pet to return (required)
   * @param sessionId session id (required)
   * @return Void
   */
  @POST("/pet/feed/{petId}")
  Void feedPet(
    @retrofit.http.Body Pet body, @retrofit.http.Header("token") String token, @retrofit.http.Query("petType") String petType, @retrofit.http.Query("status") String status, @retrofit.http.Path("petId") Long petId, String sessionId
  );

  /**
   * Find pet by ID
   * Async method
   * @param body Pet object that needs to be added to the store (required)
   * @param token status (required)
   * @param petType type of food (required)
   * @param status status (required)
   * @param petId ID of pet to return (required)
   * @param sessionId session id (required)
   * @param cb callback method
   */
  @POST("/pet/feed/{petId}")
  void feedPet(
    @retrofit.http.Body Pet body, @retrofit.http.Header("token") String token, @retrofit.http.Query("petType") String petType, @retrofit.http.Query("status") String status, @retrofit.http.Path("petId") Long petId, String sessionId, Callback<Void> cb
  );
  /**
   * Finds Pets by status
   * Sync method
   * Multiple status values can be provided with comma separated strings
   * @param status Status values that need to be considered for filter (required)
   * @return List&lt;Pet&gt;
   */
  @GET("/pet/findByStatus")
  List<Pet> findPetsByStatus(
    @retrofit.http.Query("status") List<String> status
  );

  /**
   * Finds Pets by status
   * Async method
   * @param status Status values that need to be considered for filter (required)
   * @param cb callback method
   */
  @GET("/pet/findByStatus")
  void findPetsByStatus(
    @retrofit.http.Query("status") List<String> status, Callback<List<Pet>> cb
  );
  /**
   * Finds Pets by tags
   * Sync method
   * Muliple tags can be provided with comma separated strings. Use\\ \\ tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by (required)
   * @return List&lt;Pet&gt;
   */
  @GET("/pet/findByTags")
  List<Pet> findPetsByTags(
    @retrofit.http.Query("tags") List<String> tags
  );

  /**
   * Finds Pets by tags
   * Async method
   * @param tags Tags to filter by (required)
   * @param cb callback method
   */
  @GET("/pet/findByTags")
  void findPetsByTags(
    @retrofit.http.Query("tags") List<String> tags, Callback<List<Pet>> cb
  );
  /**
   * get Parrots
   * Sync method
   * 
   * @return List&lt;Object&gt;
   */
  @GET("/parrot")
  List<Object> getParrots();
    

  /**
   * get Parrots
   * Async method
   * @param cb callback method
   */
  @GET("/parrot")
  void getParrots(
    Callback<List<Object>> cb
  );
  /**
   * Find pet by ID
   * Sync method
   * Returns a single pet
   * @param petId ID of pet to return (required)
   * @return Pet
   */
  @GET("/pet/{petId}")
  Pet getPetById(
    @retrofit.http.Path("petId") Long petId
  );

  /**
   * Find pet by ID
   * Async method
   * @param petId ID of pet to return (required)
   * @param cb callback method
   */
  @GET("/pet/{petId}")
  void getPetById(
    @retrofit.http.Path("petId") Long petId, Callback<Pet> cb
  );
  /**
   * update parrots
   * Sync method
   * 
   * @param body  (optional)
   * @return InlineResponse200
   */
  @PUT("/parrot")
  InlineResponse200 updateParrots(
    @retrofit.http.Body Body1 body
  );

  /**
   * update parrots
   * Async method
   * @param body  (optional)
   * @param cb callback method
   */
  @PUT("/parrot")
  void updateParrots(
    @retrofit.http.Body Body1 body, Callback<InlineResponse200> cb
  );
  /**
   * Update an existing pet
   * Sync method
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @return Void
   */
  @PUT("/pet")
  Void updatePet(
    @retrofit.http.Body Pet body
  );

  /**
   * Update an existing pet
   * Async method
   * @param body Pet object that needs to be added to the store (required)
   * @param cb callback method
   */
  @PUT("/pet")
  void updatePet(
    @retrofit.http.Body Pet body, Callback<Void> cb
  );
  /**
   * Updates a pet in the store with form data
   * Sync method
   * 
   * @param petId ID of pet that needs to be updated (required)
   * @param name  (optional)
   * @param status  (optional)
   * @return Void
   */
  @retrofit.http.FormUrlEncoded
  @POST("/pet/{petId}")
  Void updatePetWithForm(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Field("name") String name, @retrofit.http.Field("status") String status
  );

  /**
   * Updates a pet in the store with form data
   * Async method
   * @param petId ID of pet that needs to be updated (required)
   * @param name  (optional)
   * @param status  (optional)
   * @param cb callback method
   */
  @retrofit.http.FormUrlEncoded
  @POST("/pet/{petId}")
  void updatePetWithForm(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Field("name") String name, @retrofit.http.Field("status") String status, Callback<Void> cb
  );
  /**
   * uploads an image
   * Sync method
   * 
   * @param petId ID of pet to update (required)
   * @param body  (optional)
   * @return ModelApiResponse
   */
  @POST("/pet/{petId}/uploadImage")
  ModelApiResponse uploadFile(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Body Object body
  );

  /**
   * uploads an image
   * Async method
   * @param petId ID of pet to update (required)
   * @param body  (optional)
   * @param cb callback method
   */
  @POST("/pet/{petId}/uploadImage")
  void uploadFile(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Body Object body, Callback<ModelApiResponse> cb
  );
}
