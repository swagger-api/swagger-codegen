package io.swagger.api;

import io.swagger.model.Dog;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
 * Swagger Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
@Path("/")
public interface DogApi  {

    /**
     * Add a new dog to the store
     *
     */
    @POST
    @Path("/dog")
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Add a new dog to the store", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public void addDog(@Valid Dog body);

    /**
     * Deletes a dog
     *
     */
    @DELETE
    @Path("/dog/{dogId}")
    @Operation(summary = "Deletes a dog", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid dog value") })
    public void deleteDog(@PathParam("dogId") Long dogId, @HeaderParam("api_key") String apiKey);

    /**
     * Find dog by ID
     *
     * Returns a single dog
     *
     */
    @GET
    @Path("/dog/{dogId}")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find dog by ID", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Dog.class))),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    public Dog getDogById(@PathParam("dogId") Long dogId);

    /**
     * Update an existing dog
     *
     */
    @PUT
    @Path("/dog")
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Update an existing dog", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Animal not found"),
        @ApiResponse(responseCode = "405", description = "Validation exception") })
    public void updateDog(@Valid Dog body);

    /**
     * Updates a dog
     *
     */
    @POST
    @Path("/dog/{dogId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Operation(summary = "Updates a dog", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public void updateDogWithForm(@PathParam("dogId") Long dogId, @Multipart(value = "name", required = false)  String name, @Multipart(value = "status", required = false)  String status);
}
