package io.swagger.api;

import io.swagger.model.Dog;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/dog")

@javax.annotation.Generated(value = "io.swagger.codegen.v3.generators.java.JavaJAXRSSpecServerCodegen", date = "2020-03-13T07:31:40.002-05:00[America/Bogota]")
public interface DogApi {

    @POST
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Add a new dog to the store", description = "", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    void addDog(@Valid Dog body);
    @DELETE
    @Path("/{dogId}")
    @Operation(summary = "Deletes a dog", description = "", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid dog value") })
    void deleteDog( @PathParam("dogId")

 @Parameter(description = "Dog id to delete") Long dogId
,  @HeaderParam("api_key") 

 String apiKey
);
    @GET
    @Path("/{dogId}")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find dog by ID", description = "Returns a single dog", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Dog.class))),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    Dog getDogById( @PathParam("dogId")

 @Parameter(description = "ID of dog to return") Long dogId
);
    @PUT
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Update an existing dog", description = "", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Animal not found"),
        @ApiResponse(responseCode = "405", description = "Validation exception") })
    void updateDog(@Valid Dog body);
    @POST
    @Path("/{dogId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Operation(summary = "Updates a dog", description = "", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    void updateDogWithForm( @PathParam("dogId")

 @Parameter(description = "ID of dog that needs to be updated") Long dogId
,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status);}
