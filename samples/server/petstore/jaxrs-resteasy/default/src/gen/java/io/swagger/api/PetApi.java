package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.PetApiService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;

import java.util.Map;
import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.inject.Inject;

import javax.validation.constraints.*;
@Path("/pet")


public class PetApi  {

    @Inject PetApiService service;

    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Add a new pet to the store", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""
        })
    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public Response addPet(@Parameter(description = "Pet object that needs to be added to the store" ,required=true) Pet body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.addPet(body,securityContext);
    }
    @DELETE
    @Path("/{petId}")
    
    
    @Operation(summary = "Deletes a pet", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""
        })
    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    public Response deletePet( @PathParam("petId") Long petId,@Parameter(description = "" )@HeaderParam("api_key") String apiKey,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.deletePet(petId,apiKey,securityContext);
    }
    @GET
    @Path("/findByStatus")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Finds Pets by status", description = "Multiple status values can be provided with comma separated strings", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""
        })
    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        
        @ApiResponse(responseCode = "400", description = "Invalid status value") })
    public Response findPetsByStatus( @NotNull  @QueryParam("status") List<String> status,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.findPetsByStatus(status,securityContext);
    }
    @GET
    @Path("/findByTags")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Finds Pets by tags", description = "Muliple tags can be provided with comma separated strings. Use\\ \\ tag1, tag2, tag3 for testing.", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""
        })
    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        
        @ApiResponse(responseCode = "400", description = "Invalid tag value") })
    public Response findPetsByTags( @NotNull  @QueryParam("tags") List<String> tags,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.findPetsByTags(tags,securityContext);
    }
    @GET
    @Path("/{petId}")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Find pet by ID", description = "Returns a single pet", security = {
        @SecurityRequirement(name = "api_key")
    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Pet.class))),
        
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    public Response getPetById( @PathParam("petId") Long petId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.getPetById(petId,securityContext);
    }
    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Update an existing pet", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""
        })
    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        
        @ApiResponse(responseCode = "404", description = "Pet not found"),
        
        @ApiResponse(responseCode = "405", description = "Validation exception") })
    public Response updatePet(@Parameter(description = "Pet object that needs to be added to the store" ,required=true) Pet body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.updatePet(body,securityContext);
    }
    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @Operation(summary = "Updates a pet in the store with form data", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""
        })
    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public Response updatePetWithForm( @PathParam("petId") Long petId,@Parameter(description = "")@FormParam("name")  String name,@Parameter(description = "")@FormParam("status")  String status,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.updatePetWithForm(petId,name,status,securityContext);
    }
    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "application/octet-stream" })
    @Produces({ "application/json" })
    @Operation(summary = "uploads an image", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""
        })
    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = ModelApiResponse.class))) })
    public Response uploadFile( @PathParam("petId") Long petId,@Parameter(description = "" ) Object body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.uploadFile(petId,body,securityContext);
    }
}
