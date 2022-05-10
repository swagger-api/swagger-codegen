package io.swagger.api;

import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;
import io.swagger.model.SubCategory;
import io.swagger.api.PetApiService;

import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import java.io.InputStream;

import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
@Path("/pet")
@RequestScoped





public class PetApi  {

  @Context SecurityContext securityContext;

  @Inject PetApiService delegate;


    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Add a new pet to the store", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public Response addPet(
@Parameter(description = "Pet object that needs to be added to the store" ,required=true) Pet body
) {
        return delegate.addPet(body, securityContext);
    }

    @DELETE
    @Path("/{petId}")
    
    
    @Operation(summary = "Deletes a pet", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    public Response deletePet(
@Parameter(description = "Pet id to delete",required=true) @PathParam("petId") Long petId
, 
@Parameter(description = "" )@HeaderParam("api_key") String apiKey
) {
        return delegate.deletePet(petId, apiKey, securityContext);
    }

    @POST
    @Path("/category")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "", description = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = ModelApiResponse.class))) })
    public Response doCategoryStuff(
@Parameter(description = "" ) SubCategory body
) {
        return delegate.doCategoryStuff(body, securityContext);
    }

    @GET
    @Path("/findByStatus")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Finds Pets by status", description = "Multiple status values can be provided with comma separated strings", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        @ApiResponse(responseCode = "400", description = "Invalid status value") })
    public Response findPetsByStatus( @NotNull 
@Parameter(description = "Status values that need to be considered for filter",required=true, schema=@Schema(allowableValues={ "available", "pending", "sold" })
)  @QueryParam("status") List<String> status
) {
        return delegate.findPetsByStatus(status, securityContext);
    }

    @GET
    @Path("/findByTags")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Finds Pets by tags", description = "Muliple tags can be provided with comma separated strings. Use\\ \\ tag1, tag2, tag3 for testing.", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        @ApiResponse(responseCode = "400", description = "Invalid tag value") })
    public Response findPetsByTags( @NotNull 
@Parameter(description = "Tags to filter by",required=true)  @QueryParam("tags") List<String> tags
) {
        return delegate.findPetsByTags(tags, securityContext);
    }

    @GET
    @Path("/{petId}")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Find pet by ID", description = "Returns a single pet", security = {
        @SecurityRequirement(name = "api_key")    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Pet.class))),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    public Response getPetById(
@Parameter(description = "ID of pet to return",required=true) @PathParam("petId") Long petId
) {
        return delegate.getPetById(petId, securityContext);
    }

    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Update an existing pet", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found"),
        @ApiResponse(responseCode = "405", description = "Validation exception") })
    public Response updatePet(
@Parameter(description = "Pet object that needs to be added to the store" ,required=true) Pet body
) {
        return delegate.updatePet(body, securityContext);
    }

    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @Operation(summary = "Updates a pet in the store with form data", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public Response updatePetWithForm(
@Parameter(description = "ID of pet that needs to be updated",required=true) @PathParam("petId") Long petId
, @Multipart(value = "name", required = false)  String name, @Multipart(value = "status", required = false)  String status) {
        return delegate.updatePetWithForm(petId, name, status, securityContext);
    }

    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "application/octet-stream" })
    @Produces({ "application/json" })
    @Operation(summary = "uploads an image", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = ModelApiResponse.class))) })
    public Response uploadFile(
@Parameter(description = "ID of pet to update",required=true) @PathParam("petId") Long petId
, 
@Parameter(description = "" ) Object body
) {
        return delegate.uploadFile(petId, body, securityContext);
    }
}
