package io.swagger.client.api;

import io.swagger.client.ApiClient;
import io.swagger.client.EncodingUtils;

import io.swagger.client.model.Dog;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

public interface DogApi extends ApiClient.Api {

  /**
   * Add a new dog to the store
   * 
   * @param body Dog object that needs to be added to the store (required)
   */
  @RequestLine("POST /dog")
  @Headers({
      "Content-Type: application/json",
      "Accept: */*",
  })
  void addDog(Dog body);
  /**
   * Deletes a dog
   * 
   * @param dogId Dog id to delete (required)
   * @param apiKey  (optional)
   */
  @RequestLine("DELETE /dog/{dogId}")
  @Headers({
      "Content-Type: application/json",
      "Accept: */*",
    "api_key: {apiKey}"
  })
  void deleteDog(@Param("dogId") Long dogId, @Param("apiKey") String apiKey);
  /**
   * Find dog by ID
   * Returns a single dog
   * @param dogId ID of dog to return (required)
   * @return Dog
   */
  @RequestLine("GET /dog/{dogId}")
  @Headers({
      "Accept: application/json",
  })
  Dog getDogById(@Param("dogId") Long dogId);
  /**
   * Update an existing dog
   * 
   * @param body Dog object that needs to be added. (required)
   */
  @RequestLine("PUT /dog")
  @Headers({
      "Content-Type: application/json",
      "Accept: */*",
  })
  void updateDog(Dog body);
  /**
   * Updates a dog
   * 
   * @param dogId ID of dog that needs to be updated (required)
   * @param name  (optional)
   * @param status  (optional)
   */
  @RequestLine("POST /dog/{dogId}")
  @Headers({
      "Content-Type: application/x-www-form-urlencoded",
      "Accept: */*",
  })
  void updateDogWithForm(@Param("dogId") Long dogId, @Param("name") String name, @Param("status") String status);
}
