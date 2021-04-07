package io.swagger.client.apis

import retrofit2.Call
import retrofit2.http.*

import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody

import io.swagger.client.models.ApiResponse
import io.swagger.client.models.Pet

interface PetApi {
  /**
   * Add a new pet to the store
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @return Call&lt;Void&gt;
   */
  @Headers(
    "Content-Type:application/json"
  )
  @POST("pet")
  fun addPet(
    @retrofit2.http.Body body: Pet
  ): Call<Void>

  /**
   * Deletes a pet
   * 
   * @param petId Pet id to delete (required)
   * @param apiKey  (optional)
   * @return Call&lt;Void&gt;
   */
  @DELETE("pet/{petId}")
  fun deletePet(
    @retrofit2.http.Path("petId") petId: kotlin.Long,
    @retrofit2.http.Header("api_key") apiKey: kotlin.String? = null
  ): Call<Void>

  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma separated strings
   * @param status Status values that need to be considered for filter (required)
   * @return Call&lt;kotlin.collections.List&lt;Pet&gt;&gt;
   */
  @GET("pet/findByStatus")
  fun findPetsByStatus(
    @retrofit2.http.Query("status") status: kotlin.collections.List<kotlin.String>
  ): Call<kotlin.collections.List<Pet>>

  /**
   * Finds Pets by tags
   * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by (required)
   * @return Call&lt;kotlin.collections.List&lt;Pet&gt;&gt;
   * @deprecated
   */
  @Deprecated("")
  @GET("pet/findByTags")
  fun findPetsByTags(
    @retrofit2.http.Query("tags") tags: kotlin.collections.List<kotlin.String>
  ): Call<kotlin.collections.List<Pet>>

  /**
   * Find pet by ID
   * Returns a single pet
   * @param petId ID of pet to return (required)
   * @return Call&lt;Pet&gt;
   */
  @GET("pet/{petId}")
  fun getPetById(
    @retrofit2.http.Path("petId") petId: kotlin.Long
  ): Call<Pet>

  /**
   * Update an existing pet
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @return Call&lt;Void&gt;
   */
  @Headers(
    "Content-Type:application/json"
  )
  @PUT("pet")
  fun updatePet(
    @retrofit2.http.Body body: Pet
  ): Call<Void>

  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated (required)
   * @param name Updated name of the pet (optional)
   * @param status Updated status of the pet (optional)
   * @return Call&lt;Void&gt;
   */
  @retrofit2.http.FormUrlEncoded
  @POST("pet/{petId}")
  fun updatePetWithForm(
    @retrofit2.http.Path("petId") petId: kotlin.Long,
    @retrofit2.http.Field("name") name: kotlin.String? = null,
    @retrofit2.http.Field("status") status: kotlin.String? = null
  ): Call<Void>

  /**
   * uploads an image
   * 
   * @param petId ID of pet to update (required)
   * @param additionalMetadata Additional data to pass to server (optional)
   * @param file file to upload (optional)
   * @return Call&lt;ApiResponse&gt;
   */
  @retrofit2.http.Multipart
  @POST("pet/{petId}/uploadImage")
  fun uploadFile(
    @retrofit2.http.Path("petId") petId: kotlin.Long,
    @retrofit2.http.Part("additionalMetadata") additionalMetadata: kotlin.String? = null,
    @retrofit2.http.Part file: MultipartBody.Part? = null
  ): Call<ApiResponse>

}
