package io.swagger.api;

import io.swagger.model.*;

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
import io.swagger.model.SubCategory;

import java.util.List;
import java.util.Map;

import java.io.InputStream;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.validation.constraints.*;
@Path("/pet")


public interface PetApi  {
   
    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Add a new pet to the store", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "405", description = "Invalid input")
         })
    Response addPet(@Parameter(description = "Pet object that needs to be added to the store" ,required=true) Pet body,@Context SecurityContext securityContext);

    @DELETE
    @Path("/{petId}")
    
    
    @Operation(summary = "Deletes a pet", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
                @ApiResponse(responseCode = "404", description = "Pet not found")
         })
    Response deletePet( @PathParam("petId") Long petId,@Parameter(description = "" )@HeaderParam("api_key") String apiKey,@Context SecurityContext securityContext);

    @POST
    @Path("/category")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "", description = "", tags={ "pet" })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = ModelApiResponse.class)))
         })
    Response doCategoryStuff(@Parameter(description = "" ) SubCategory body,@Context SecurityContext securityContext);

    @GET
    @Path("/findByStatus")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Finds Pets by status", description = "Multiple status values can be provided with comma separated strings", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
                @ApiResponse(responseCode = "400", description = "Invalid status value")
         })
    Response findPetsByStatus( @NotNull @QueryParam("status") List<String> status,@Context SecurityContext securityContext);

    @GET
    @Path("/findByTags")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Finds Pets by tags", description = "Muliple tags can be provided with comma separated strings. Use\\ \\ tag1, tag2, tag3 for testing.", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
                @ApiResponse(responseCode = "400", description = "Invalid tag value")
         })
    Response findPetsByTags( @NotNull @QueryParam("tags") List<String> tags,@Context SecurityContext securityContext);

    @GET
    @Path("/{petId}")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Find pet by ID", description = "Returns a single pet", security = {
        @SecurityRequirement(name = "api_key")    }, tags={ "pet" })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Pet.class))),
                @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
                @ApiResponse(responseCode = "404", description = "Pet not found")
         })
    Response getPetById( @PathParam("petId") Long petId,@Context SecurityContext securityContext);

    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Update an existing pet", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
                @ApiResponse(responseCode = "404", description = "Pet not found"),
                @ApiResponse(responseCode = "405", description = "Validation exception")
         })
    Response updatePet(@Parameter(description = "Pet object that needs to be added to the store" ,required=true) Pet body,@Context SecurityContext securityContext);

    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @Operation(summary = "Updates a pet in the store with form data", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "405", description = "Invalid input")
         })
    Response updatePetWithForm( @PathParam("petId") Long petId,@Parameter(description = "")@FormParam("name")  String name,@Parameter(description = "")@FormParam("status")  String status,@Context SecurityContext securityContext);

    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "application/octet-stream" })
    @Produces({ "application/json" })
    @Operation(summary = "uploads an image", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = ModelApiResponse.class)))
         })
    Response uploadFile( @PathParam("petId") Long petId,@Parameter(description = "" ) Object body,@Context SecurityContext securityContext);

}
