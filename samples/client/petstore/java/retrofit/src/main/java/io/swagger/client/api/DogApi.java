package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import io.swagger.client.model.Dog;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface DogApi {
  /**
   * Add a new dog to the store
   * Sync method
   * 
   * @param body Dog object that needs to be added to the store (required)
   * @return Void
   */
  @POST("/dog")
  Void addDog(
    @retrofit.http.Body Dog body
  );

  /**
   * Add a new dog to the store
   * Async method
   * @param body Dog object that needs to be added to the store (required)
   * @param cb callback method
   */
  @POST("/dog")
  void addDog(
    @retrofit.http.Body Dog body, Callback<Void> cb
  );
  /**
   * Deletes a dog
   * Sync method
   * 
   * @param dogId Dog id to delete (required)
   * @param apiKey  (optional)
   * @return Void
   */
  @DELETE("/dog/{dogId}")
  Void deleteDog(
    @retrofit.http.Path("dogId") Long dogId, @retrofit.http.Header("api_key") String apiKey
  );

  /**
   * Deletes a dog
   * Async method
   * @param dogId Dog id to delete (required)
   * @param apiKey  (optional)
   * @param cb callback method
   */
  @DELETE("/dog/{dogId}")
  void deleteDog(
    @retrofit.http.Path("dogId") Long dogId, @retrofit.http.Header("api_key") String apiKey, Callback<Void> cb
  );
  /**
   * Find dog by ID
   * Sync method
   * Returns a single dog
   * @param dogId ID of dog to return (required)
   * @return Dog
   */
  @GET("/dog/{dogId}")
  Dog getDogById(
    @retrofit.http.Path("dogId") Long dogId
  );

  /**
   * Find dog by ID
   * Async method
   * @param dogId ID of dog to return (required)
   * @param cb callback method
   */
  @GET("/dog/{dogId}")
  void getDogById(
    @retrofit.http.Path("dogId") Long dogId, Callback<Dog> cb
  );
  /**
   * Update an existing dog
   * Sync method
   * 
   * @param body Dog object that needs to be added. (required)
   * @return Void
   */
  @PUT("/dog")
  Void updateDog(
    @retrofit.http.Body Dog body
  );

  /**
   * Update an existing dog
   * Async method
   * @param body Dog object that needs to be added. (required)
   * @param cb callback method
   */
  @PUT("/dog")
  void updateDog(
    @retrofit.http.Body Dog body, Callback<Void> cb
  );
  /**
   * Updates a dog
   * Sync method
   * 
   * @param dogId ID of dog that needs to be updated (required)
   * @param name  (optional)
   * @param status  (optional)
   * @return Void
   */
  @retrofit.http.FormUrlEncoded
  @POST("/dog/{dogId}")
  Void updateDogWithForm(
    @retrofit.http.Path("dogId") Long dogId, @retrofit.http.Field("name") String name, @retrofit.http.Field("status") String status
  );

  /**
   * Updates a dog
   * Async method
   * @param dogId ID of dog that needs to be updated (required)
   * @param name  (optional)
   * @param status  (optional)
   * @param cb callback method
   */
  @retrofit.http.FormUrlEncoded
  @POST("/dog/{dogId}")
  void updateDogWithForm(
    @retrofit.http.Path("dogId") Long dogId, @retrofit.http.Field("name") String name, @retrofit.http.Field("status") String status, Callback<Void> cb
  );
}
