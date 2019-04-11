package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import io.swagger.client.model.Animal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface AnimalApi {
  /**
   * Add a new animal to the store
   * Sync method
   * 
   * @param body Animal object that needs to be added to the store (required)
   * @return Void
   */
  @POST("/animal")
  Void addAnimal(
    @retrofit.http.Body Animal body
  );

  /**
   * Add a new animal to the store
   * Async method
   * @param body Animal object that needs to be added to the store (required)
   * @param cb callback method
   */
  @POST("/animal")
  void addAnimal(
    @retrofit.http.Body Animal body, Callback<Void> cb
  );
  /**
   * Deletes a animal
   * Sync method
   * 
   * @param animalId Animal id to delete (required)
   * @param apiKey  (optional)
   * @return Void
   */
  @DELETE("/animal/{animalId}")
  Void deleteAnimal(
    @retrofit.http.Path("animalId") Long animalId, @retrofit.http.Header("api_key") String apiKey
  );

  /**
   * Deletes a animal
   * Async method
   * @param animalId Animal id to delete (required)
   * @param apiKey  (optional)
   * @param cb callback method
   */
  @DELETE("/animal/{animalId}")
  void deleteAnimal(
    @retrofit.http.Path("animalId") Long animalId, @retrofit.http.Header("api_key") String apiKey, Callback<Void> cb
  );
  /**
   * Find animal by ID
   * Sync method
   * Returns a single animal
   * @param animalId ID of pet to return (required)
   * @return Animal
   */
  @GET("/animal/{animalId}")
  Animal getAnimalById(
    @retrofit.http.Path("animalId") Long animalId
  );

  /**
   * Find animal by ID
   * Async method
   * @param animalId ID of pet to return (required)
   * @param cb callback method
   */
  @GET("/animal/{animalId}")
  void getAnimalById(
    @retrofit.http.Path("animalId") Long animalId, Callback<Animal> cb
  );
  /**
   * Update an existing animal
   * Sync method
   * 
   * @param body Animal object that needs to be added. (required)
   * @return Void
   */
  @PUT("/animal")
  Void updateAnimal(
    @retrofit.http.Body Animal body
  );

  /**
   * Update an existing animal
   * Async method
   * @param body Animal object that needs to be added. (required)
   * @param cb callback method
   */
  @PUT("/animal")
  void updateAnimal(
    @retrofit.http.Body Animal body, Callback<Void> cb
  );
  /**
   * Updates a animal
   * Sync method
   * 
   * @param animalId ID of animal that needs to be updated (required)
   * @param name  (optional)
   * @param status  (optional)
   * @return Void
   */
  @retrofit.http.FormUrlEncoded
  @POST("/animal/{animalId}")
  Void updateAnimalWithForm(
    @retrofit.http.Path("animalId") Long animalId, @retrofit.http.Field("name") String name, @retrofit.http.Field("status") String status
  );

  /**
   * Updates a animal
   * Async method
   * @param animalId ID of animal that needs to be updated (required)
   * @param name  (optional)
   * @param status  (optional)
   * @param cb callback method
   */
  @retrofit.http.FormUrlEncoded
  @POST("/animal/{animalId}")
  void updateAnimalWithForm(
    @retrofit.http.Path("animalId") Long animalId, @retrofit.http.Field("name") String name, @retrofit.http.Field("status") String status, Callback<Void> cb
  );
}
