package io.swagger.api;

import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;

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

@Path("/pet")

@javax.annotation.Generated(value = "io.swagger.codegen.v3.generators.java.JavaJAXRSSpecServerCodegen", date = "2020-02-20T00:57:07.355-05:00[America/Bogota]")
public interface PetApi {

    @POST
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Add a new pet to the store", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    void addPet(@Valid Pet body);
    @DELETE
    @Path("/{petId}")
    @Operation(summary = "Deletes a pet", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    void deletePet( @PathParam("petId")

 @Parameter(description = "Pet id to delete") Long petId
,  @HeaderParam("api_key") 

 String apiKey
);
    @POST
    @Path("/feed/{petId}")
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Find pet by ID", description = "schedule pet feeding", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    void feedPet(@Valid Pet body, @NotNull  @HeaderParam("token") 

 @Parameter(description = "status") String token
, @NotNull  @QueryParam("petType") 

 @Parameter(description = "type of food")  String petType
, @NotNull  @QueryParam("status") 

 @Parameter(description = "status")  String status
, @PathParam("petId")

 @Parameter(description = "ID of pet to return") Long petId
,);
    @GET
    @Path("/findByStatus")
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Finds Pets by status", description = "Multiple status values can be provided with comma separated strings", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        @ApiResponse(responseCode = "400", description = "Invalid status value") })
    List<Pet> findPetsByStatus( @NotNull  @QueryParam("status") 

 @Parameter(description = "Status values that need to be considered for filter")  List<String> status
);
    @GET
    @Path("/findByTags")
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Finds Pets by tags", description = "Muliple tags can be provided with comma separated strings. Use\\ \\ tag1, tag2, tag3 for testing.", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        @ApiResponse(responseCode = "400", description = "Invalid tag value") })
    List<Pet> findPetsByTags( @NotNull  @QueryParam("tags") 

 @Parameter(description = "Tags to filter by")  List<String> tags
);
    @GET
    @Path("/{petId}")
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Find pet by ID", description = "Returns a single pet", security = {
        @SecurityRequirement(name = "api_key")    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Pet.class))),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    Pet getPetById( @PathParam("petId")

 @Parameter(description = "ID of pet to return") Long petId
);
    @PUT
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Update an existing pet", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found"),
        @ApiResponse(responseCode = "405", description = "Validation exception") })
    void updatePet(@Valid Pet body);
    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Operation(summary = "Updates a pet in the store with form data", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    void updatePetWithForm( @PathParam("petId")

 @Parameter(description = "ID of pet that needs to be updated") Long petId
,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status);
    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "application/octet-stream" })
    @Produces({ "application/json" })
    @Operation(summary = "uploads an image", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = ModelApiResponse.class))) })
    ModelApiResponse uploadFile( @PathParam("petId")

 @Parameter(description = "ID of pet to update") Long petId
,@Valid Object body);}
