package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;



import retrofit2.Call;
import retrofit2.http.*;

import okhttp3.RequestBody;

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

import java.util.concurrent.*;
import retrofit2.Response;

public interface PetApi {
  /**
   * Add a new parrow to the store
   * 
   * @param body  (optional)
   * @return Call&lt;InlineResponse2001&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @POST("parrot")
  CompletionStage<Response<InlineResponse2001>> addParrot(
                    @retrofit2.http.Body Body2 body    
  );

  /**
   * Add a new pet to the store
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @POST("pet")
  CompletionStage<Response<Void>> addPet(
                    @retrofit2.http.Body Pet body    
  );

  /**
   * Deletes a pet
   * 
   * @param petId Pet id to delete (required)
   * @param apiKey  (optional)
   * @return Call&lt;Void&gt;
   */
  @DELETE("pet/{petId}")
  CompletionStage<Response<Void>> deletePet(
            @retrofit2.http.Path("petId") Long petId            ,             @retrofit2.http.Header("api_key") String apiKey        
  );

  /**
   * Find pet by ID
   * schedule pet feeding
   * @param body Pet object that needs to be added to the store (required)
   * @param token status (required)
   * @param petType type of food (required)
   * @param status status (required)
   * @param petId ID of pet to return (required)
   * @param sessionId session id (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @POST("pet/feed/{petId}")
  CompletionStage<Response<Void>> feedPet(
                    @retrofit2.http.Body Pet body    ,             @retrofit2.http.Header("token") String token        ,     @retrofit2.http.Query("petType") String petType                ,     @retrofit2.http.Query("status") String status                ,         @retrofit2.http.Path("petId") Long petId            ,                     String sessionId
  );

  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma separated strings
   * @param status Status values that need to be considered for filter (required)
   * @return Call&lt;List&lt;Pet&gt;&gt;
   */
  @GET("pet/findByStatus")
  CompletionStage<Response<List<Pet>>> findPetsByStatus(
        @retrofit2.http.Query("status") List<String> status                
  );

  /**
   * Finds Pets by tags
   * Muliple tags can be provided with comma separated strings. Use\\ \\ tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by (required)
   * @return Call&lt;List&lt;Pet&gt;&gt;
   */
  @GET("pet/findByTags")
  CompletionStage<Response<List<Pet>>> findPetsByTags(
        @retrofit2.http.Query("tags") List<String> tags                
  );

  /**
   * get Parrots
   * 
   * @return Call&lt;List&lt;Object&gt;&gt;
   */
  @GET("parrot")
  CompletionStage<Response<List<Object>>> getParrots();
    

  /**
   * Find pet by ID
   * Returns a single pet
   * @param petId ID of pet to return (required)
   * @return Call&lt;Pet&gt;
   */
  @GET("pet/{petId}")
  CompletionStage<Response<Pet>> getPetById(
            @retrofit2.http.Path("petId") Long petId            
  );

  /**
   * update parrots
   * 
   * @param body  (optional)
   * @return Call&lt;InlineResponse200&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PUT("parrot")
  CompletionStage<Response<InlineResponse200>> updateParrots(
                    @retrofit2.http.Body Body1 body    
  );

  /**
   * Update an existing pet
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PUT("pet")
  CompletionStage<Response<Void>> updatePet(
                    @retrofit2.http.Body Pet body    
  );

  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated (required)
   * @param name  (optional)
   * @param status  (optional)
   * @return Call&lt;Void&gt;
   */
  @retrofit2.http.FormUrlEncoded
  @POST("pet/{petId}")
  CompletionStage<Response<Void>> updatePetWithForm(
            @retrofit2.http.Path("petId") Long petId            ,                     @retrofit2.http.Field("name") String name,                     @retrofit2.http.Field("status") String status
  );

  /**
   * uploads an image
   * 
   * @param petId ID of pet to update (required)
   * @param body  (optional)
   * @return Call&lt;ModelApiResponse&gt;
   */
  @Headers({
    "Content-Type:application/octet-stream"
  })
  @POST("pet/{petId}/uploadImage")
  CompletionStage<Response<ModelApiResponse>> uploadFile(
            @retrofit2.http.Path("petId") Long petId            ,                 @retrofit2.http.Body Object body    
  );

}
