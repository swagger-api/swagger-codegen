package io.swagger.client.api;

import io.swagger.client.ApiClient;
import io.swagger.client.EncodingUtils;

import io.swagger.client.model.Animal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

public interface AnimalApi extends ApiClient.Api {

  /**
   * Add a new animal to the store
   * 
   * @param body Animal object that needs to be added to the store (required)
   */
  @RequestLine("POST /animal")
  @Headers({
      "Accept: */*",
  })
  void addAnimal(Animal body);
  /**
   * Deletes a animal
   * 
   * @param animalId Animal id to delete (required)
   * @param apiKey  (optional)
   */
  @RequestLine("DELETE /animal/{animalId}")
  @Headers({
      "Accept: */*",
    "api_key: {apiKey}"
  })
  void deleteAnimal(@Param("animalId") Long animalId, @Param("apiKey") String apiKey);
  /**
   * Find animal by ID
   * Returns a single animal
   * @param animalId ID of pet to return (required)
   * @return Animal
   */
  @RequestLine("GET /animal/{animalId}")
  @Headers({
      "Accept: */*",
  })
  Animal getAnimalById(@Param("animalId") Long animalId);
  /**
   * Update an existing animal
   * 
   * @param body Animal object that needs to be added. (required)
   */
  @RequestLine("PUT /animal")
  @Headers({
      "Accept: */*",
  })
  void updateAnimal(Animal body);
  /**
   * Updates a animal
   * 
   * @param animalId ID of animal that needs to be updated (required)
   * @param name  (optional)
   * @param status  (optional)
   */
  @RequestLine("POST /animal/{animalId}")
  @Headers({
      "Accept: */*",
  })
  void updateAnimalWithForm(@Param("animalId") Long animalId, @Param("name") String name, @Param("status") String status);
}
