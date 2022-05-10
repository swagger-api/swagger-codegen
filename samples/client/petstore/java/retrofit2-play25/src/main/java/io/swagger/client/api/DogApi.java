package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;



import retrofit2.Call;
import retrofit2.http.*;

import okhttp3.RequestBody;

import io.swagger.client.model.Dog;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.concurrent.*;
import retrofit2.Response;

public interface DogApi {
  /**
   * Add a new dog to the store
   * 
   * @param body Dog object that needs to be added to the store (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @POST("dog")
  CompletionStage<Response<Void>> addDog(
                    @retrofit2.http.Body Dog body    
  );

  /**
   * Deletes a dog
   * 
   * @param dogId Dog id to delete (required)
   * @param apiKey  (optional)
   * @return Call&lt;Void&gt;
   */
  @DELETE("dog/{dogId}")
  CompletionStage<Response<Void>> deleteDog(
            @retrofit2.http.Path("dogId") Long dogId            ,             @retrofit2.http.Header("api_key") String apiKey        
  );

  /**
   * Find dog by ID
   * Returns a single dog
   * @param dogId ID of dog to return (required)
   * @return Call&lt;Dog&gt;
   */
  @GET("dog/{dogId}")
  CompletionStage<Response<Dog>> getDogById(
            @retrofit2.http.Path("dogId") Long dogId            
  );

  /**
   * Update an existing dog
   * 
   * @param body Dog object that needs to be added. (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PUT("dog")
  CompletionStage<Response<Void>> updateDog(
                    @retrofit2.http.Body Dog body    
  );

  /**
   * Updates a dog
   * 
   * @param dogId ID of dog that needs to be updated (required)
   * @param name  (optional)
   * @param status  (optional)
   * @return Call&lt;Void&gt;
   */
  @retrofit2.http.FormUrlEncoded
  @POST("dog/{dogId}")
  CompletionStage<Response<Void>> updateDogWithForm(
            @retrofit2.http.Path("dogId") Long dogId            ,                     @retrofit2.http.Field("name") String name,                     @retrofit2.http.Field("status") String status
  );

}
