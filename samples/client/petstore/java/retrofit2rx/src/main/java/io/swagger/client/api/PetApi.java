package io.swagger.client.api;

import java.util.List;

import io.swagger.client.CollectionFormats.CSVParams;
import io.swagger.client.model.ModelApiResponse;
import io.swagger.client.model.Pet;
import okhttp3.RequestBody;
import retrofit2.http.Body;
import retrofit2.http.DELETE;
import retrofit2.http.Field;
import retrofit2.http.FormUrlEncoded;
import retrofit2.http.GET;
import retrofit2.http.Header;
import retrofit2.http.Multipart;
import retrofit2.http.POST;
import retrofit2.http.PUT;
import retrofit2.http.Part;
import retrofit2.http.Path;
import retrofit2.http.Query;
import rx.Observable;

public interface PetApi {
    /**
     * Add a new pet to the store
     *
     * @param body Pet object that needs to be added to the store (required)
     * @return Call<Void>
     */

    @POST("pet")
    Observable<Void> addPet(
            @Body Pet body
    );

    /**
     * Deletes a pet
     *
     * @param petId  Pet id to delete (required)
     * @param apiKey (optional)
     * @return Call<Void>
     */

    @DELETE("pet/{petId}")
    Observable<Void> deletePet(
            @Path("petId") Long petId, @Header("api_key") String apiKey
    );

    /**
     * Finds Pets by status Multiple status values can be provided with comma separated strings
     *
     * @param status Status values that need to be considered for filter (required)
     * @return Call<List<Pet>>
     */

    @GET("pet/findByStatus")
    Observable<List<Pet>> findPetsByStatus(
            @Query("status") CSVParams status
    );

    /**
     * Finds Pets by tags Multiple tags can be provided with comma separated strings. Use tag1,
     * tag2, tag3 for testing.
     *
     * @param tags Tags to filter by (required)
     * @return Call<List<Pet>>
     */

    @GET("pet/findByTags")
    Observable<List<Pet>> findPetsByTags(
            @Query("tags") CSVParams tags
    );

    /**
     * Find pet by ID Returns a single pet
     *
     * @param petId ID of pet to return (required)
     * @return Call<Pet>
     */

    @GET("pet/{petId}")
    Observable<Pet> getPetById(
            @Path("petId") Long petId
    );

    /**
     * Update an existing pet
     *
     * @param body Pet object that needs to be added to the store (required)
     * @return Call<Void>
     */

    @PUT("pet")
    Observable<Void> updatePet(
            @Body Pet body
    );

    /**
     * Updates a pet in the store with form data
     *
     * @param petId  ID of pet that needs to be updated (required)
     * @param name   Updated name of the pet (optional)
     * @param status Updated status of the pet (optional)
     * @return Call<Void>
     */

    @FormUrlEncoded
    @POST("pet/{petId}")
    Observable<Void> updatePetWithForm(
            @Path("petId") Long petId, @Field("name") String name, @Field("status") String status
    );

    /**
     * uploads an image
     *
     * @param petId              ID of pet to update (required)
     * @param additionalMetadata Additional data to pass to server (optional)
     * @param file               file to upload (optional)
     * @return Call<ModelApiResponse>
     */

    @Multipart
    @POST("pet/{petId}/uploadImage")
    Observable<ModelApiResponse> uploadFile(
            @Path("petId") Long petId, @Part("additionalMetadata") String additionalMetadata, @Part("file\"; filename=\"file\"") RequestBody file
    );

}
