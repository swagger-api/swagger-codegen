package io.swagger.api;

import java.io.File;
import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;

@Path("/pet")

@Api(description = "the pet API")




public interface  PetApi  {

    @POST
    
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Add a new pet to the store", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input") })
    public Response addPet(Pet body);

    @DELETE
    @Path("/{petId}")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Deletes a pet", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid pet value") })
    public Response deletePet(@PathParam("petId") Long petId,@HeaderParam("api_key") String apiKey);

    @GET
    @Path("/findByStatus")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma separated strings", response = Pet.class, responseContainer = "List", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid status value", responseContainer = "List") })
    public Response findPetsByStatus(@QueryParam("status") List<String> status);

    @GET
    @Path("/findByTags")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Finds Pets by tags", notes = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.", response = Pet.class, responseContainer = "List", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid tag value", responseContainer = "List") })
    public Response findPetsByTags(@QueryParam("tags") List<String> tags);

    @GET
    @Path("/{petId}")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Find pet by ID", notes = "Returns a single pet", response = Pet.class, authorizations = {
        @Authorization(value = "api_key")
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied"),
        @ApiResponse(code = 404, message = "Pet not found") })
    public Response getPetById(@PathParam("petId") Long petId);

    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Update an existing pet", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied"),
        @ApiResponse(code = 404, message = "Pet not found"),
        @ApiResponse(code = 405, message = "Validation exception") })
    public Response updatePet(Pet body);

    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Updates a pet in the store with form data", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input") })
    public Response updatePetWithForm(@PathParam("petId") Long petId,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status);

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
        @ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class) })
    public Response uploadFile(@PathParam("petId") Long petId,@FormParam(value = "additionalMetadata")  String additionalMetadata, @FormParam(value = "file") java.io.InputStream fileInputStream);
}

