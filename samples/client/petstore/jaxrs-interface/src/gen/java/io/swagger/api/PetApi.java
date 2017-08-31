package io.swagger.api;

import java.io.File;
import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;
import javax.validation.constraints.*;

@Path("/pet")
@Api(description = "the pet API")
public interface PetApi {

    @POST
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Add a new pet to the store", notes = "", response = void.class, authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input", response = void.class)
    })
    void addPet(Pet body);

    @DELETE
    @Path("/{petId}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Deletes a pet", notes = "", response = void.class, authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid pet value", response = void.class)
    })
    void deletePet(@PathParam("petId") @ApiParam("Pet id to delete") Long petId,@HeaderParam("api_key") String apiKey);

    @GET
    @Path("/findByStatus")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma separated strings", response = List<Pet>.class, responseContainer = "List", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = List<Pet>.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid status value", response = List<Pet>.class, responseContainer = "List")
    })
    List<List<Pet>> findPetsByStatus(@QueryParam("status") @NotNull  List<String> status);

    @GET
    @Path("/findByTags")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Finds Pets by tags", notes = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.", response = List<Pet>.class, responseContainer = "List", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = List<Pet>.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid tag value", response = List<Pet>.class, responseContainer = "List")
    })
    List<List<Pet>> findPetsByTags(@QueryParam("tags") @NotNull  List<String> tags);

    @GET
    @Path("/{petId}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Find pet by ID", notes = "Returns a single pet", response = Pet.class, authorizations = {
        @Authorization(value = "api_key")
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Pet.class),
        @ApiResponse(code = 404, message = "Pet not found", response = Pet.class)
    })
    Pet getPetById(@PathParam("petId") @ApiParam("ID of pet to return") Long petId);

    @PUT
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Update an existing pet", notes = "", response = void.class, authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = void.class),
        @ApiResponse(code = 404, message = "Pet not found", response = void.class),
        @ApiResponse(code = 405, message = "Validation exception", response = void.class)
    })
    void updatePet(Pet body);

    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Updates a pet in the store with form data", notes = "", response = void.class, authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input", response = void.class)
    })
    void updatePetWithForm(@PathParam("petId") @ApiParam("ID of pet that needs to be updated") Long petId,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status);

    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @ApiOperation(value = "uploads an image", notes = "", response = ModelApiResponse.class, authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class)
    })
    ModelApiResponse uploadFile(@PathParam("petId") @ApiParam("ID of pet to update") Long petId,@FormParam(value = "additionalMetadata")  String additionalMetadata, @FormParam(value = "file") InputStream fileInputStream,
   @FormParam(value = "file") Attachment fileDetail);
}

