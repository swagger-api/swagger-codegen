package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.PetApiService;
import io.swagger.api.factories.PetApiServiceFactory;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import java.io.File;
import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;
import io.swagger.model.SubCategory;

import java.util.Map;
import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import org.glassfish.jersey.media.multipart.FormDataParam;

import javax.servlet.ServletConfig;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.validation.constraints.*;


@Path("/pet")


public class PetApi  {
   private final PetApiService delegate;

   public PetApi(@Context ServletConfig servletContext) {
      PetApiService delegate = null;

      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("PetApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (PetApiService) Class.forName(implClass).newInstance();
            } catch (Exception e) {
               throw new RuntimeException(e);
            }
         } 
      }

      if (delegate == null) {
         delegate = PetApiServiceFactory.getPetApi();
      }

      this.delegate = delegate;
   }

    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Add a new pet to the store", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            "write:pets",
"read:pets"        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public Response addPet(@Parameter(in = ParameterIn.DEFAULT, description = "Pet object that needs to be added to the store" ,required=true) Pet body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.addPet(body,securityContext);
    }
    @DELETE
    @Path("/{petId}")
    
    
    @Operation(summary = "Deletes a pet", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            "write:pets",
"read:pets"        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid pet value") })
    public Response deletePet(@Parameter(in = ParameterIn.PATH, description = "Pet id to delete",required=true) @PathParam("petId") Long petId
,
@Parameter(in = ParameterIn.HEADER, description = "" )@HeaderParam("api_key") String apiKey

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deletePet(petId,apiKey,securityContext);
    }
    @POST
    @Path("/category")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "", description = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ModelApiResponse.class))) })
    public Response doCategoryStuff(@Parameter(in = ParameterIn.DEFAULT, description = "" ) SubCategory body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.doCategoryStuff(body,securityContext);
    }
    @GET
    @Path("/findByStatus")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Finds Pets by status", description = "Multiple status values can be provided with comma separated strings", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            "write:pets",
"read:pets"        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(mediaType = "application/xml", array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        
        @ApiResponse(responseCode = "400", description = "Invalid status value") })
    public Response findPetsByStatus(@Parameter(in = ParameterIn.QUERY, description = "Status values that need to be considered for filter",required=true, schema=@Schema(allowableValues={ "available", "pending", "sold" })
) @QueryParam("status") List<String> status
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findPetsByStatus(status,securityContext);
    }
    @GET
    @Path("/findByTags")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Finds Pets by tags", description = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            "write:pets",
"read:pets"        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(mediaType = "application/xml", array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        
        @ApiResponse(responseCode = "400", description = "Invalid tag value") })
    public Response findPetsByTags(@Parameter(in = ParameterIn.QUERY, description = "Tags to filter by",required=true) @QueryParam("tags") List<String> tags
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findPetsByTags(tags,securityContext);
    }
    @GET
    @Path("/{petId}")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find pet by ID", description = "Returns a single pet", security = {
        @SecurityRequirement(name = "api_key")    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(mediaType = "application/xml", schema = @Schema(implementation = Pet.class))),
        
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    public Response getPetById(@Parameter(in = ParameterIn.PATH, description = "ID of pet to return",required=true) @PathParam("petId") Long petId
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getPetById(petId,securityContext);
    }
    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Update an existing pet", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            "write:pets",
"read:pets"        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        
        @ApiResponse(responseCode = "404", description = "Pet not found"),
        
        @ApiResponse(responseCode = "405", description = "Validation exception") })
    public Response updatePet(@Parameter(in = ParameterIn.DEFAULT, description = "Pet object that needs to be added to the store" ,required=true) Pet body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updatePet(body,securityContext);
    }
    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @Operation(summary = "Updates a pet in the store with form data", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            "write:pets",
"read:pets"        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public Response updatePetWithForm(@Parameter(in = ParameterIn.PATH, description = "ID of pet that needs to be updated",required=true) @PathParam("petId") Long petId
,@Parameter(description = "")  @FormParam("name")  String name
,@Parameter(description = "")  @FormParam("status")  String status
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updatePetWithForm(petId,name,status,securityContext);
    }
    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @Operation(summary = "uploads an image", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            "write:pets",
"read:pets"        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ModelApiResponse.class))) })
    public Response uploadFile(@Parameter(in = ParameterIn.PATH, description = "ID of pet to update",required=true) @PathParam("petId") Long petId
,@Parameter(description = "")  @FormParam("additionalMetadata")  String additionalMetadata
,@FormDataParam("file") InputStream fileInputStream,
            @FormDataParam("file") FormDataContentDisposition fileDetail
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.uploadFile(petId,additionalMetadata,fileInputStream,fileDetail,securityContext);
    }
}
