package io.swagger.api;

import java.io.File;
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

@javax.annotation.Generated(value = "io.swagger.codegen.v3.generators.java.JavaJAXRSSpecServerCodegen", date = "2019-02-14T23:02:53.126-05:00[America/Bogota]")
public interface PetApi {

    @POST
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Add a new pet to the store", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    void addPet(@Valid Pet body);
    @DELETE
    @Path("/{petId}")
    @Operation(summary = "Deletes a pet", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid pet value") })
    void deletePet( @PathParam("petId")

 @Parameter(description = "Pet id to delete") Integer petId
,  @HeaderParam("api_key") 

 String apiKey
);
    @GET
    @Path("/findByStatus")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Finds Pets by status", description = "Multiple status values can be provided with comma separated strings", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        @ApiResponse(responseCode = "400", description = "Invalid status value") })
    List<Pet> findPetsByStatus( @NotNull  @QueryParam("status") 

 @Parameter(description = "Status values that need to be considered for filter")  List<String> status
);
    @GET
    @Path("/findByTags")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Finds Pets by tags", description = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        @ApiResponse(responseCode = "400", description = "Invalid tag value") })
    List<Pet> findPetsByTags( @NotNull  @QueryParam("tags") 

 @Parameter(description = "Tags to filter by")  List<String> tags
);
    @GET
    @Path("/{petId}")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find pet by ID", description = "Returns a single pet", security = {
        @SecurityRequirement(name = "api_key")    }, tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Pet.class))),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    Pet getPetById( @PathParam("petId")

 @Parameter(description = "ID of pet to return") Integer petId
);
    @PUT
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Update an existing pet", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={  })
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
            ""        })    }, tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    void updatePetWithForm( @PathParam("petId")

 @Parameter(description = "ID of pet that needs to be updated") Integer petId
,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status);
    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @Operation(summary = "uploads an image", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = ModelApiResponse.class))) })
    ModelApiResponse uploadFile( @PathParam("petId")

 @Parameter(description = "ID of pet to update") Integer petId
,@FormParam(value = "additionalMetadata")  String additionalMetadata, @FormParam(value = "file") InputStream fileInputStream,
   @FormParam(value = "file") Attachment fileDetail);}
