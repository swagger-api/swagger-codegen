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

import io.swagger.model.Body1;
import io.swagger.model.Body2;
import io.swagger.model.InlineResponse200;
import io.swagger.model.InlineResponse2001;
import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;

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


@Path("/Pet")


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
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "Add a new parrow to the store", description = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input"),
        
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = InlineResponse2001.class))) })
    public Response addParrot(@Parameter(in = ParameterIn.DEFAULT, description = "" ) Body2 body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.addParrot(body,securityContext);
    }
    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Add a new pet to the store", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public Response addPet(@Parameter(in = ParameterIn.DEFAULT, description = "Pet object that needs to be added to the store" ,required=true) Pet body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.addPet(body,securityContext);
    }
    @DELETE
    
    
    
    @Operation(summary = "Deletes a pet", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    public Response deletePet(@Parameter(in = ParameterIn.PATH, description = "Pet id to delete",required=true) @PathParam("petId") Long petId
,
@Parameter(in = ParameterIn.HEADER, description = "" )@HeaderParam("api_key") String apiKey

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deletePet(petId,apiKey,securityContext);
    }
    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Find pet by ID", description = "schedule pet feeding", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public Response feedPet(@Parameter(in = ParameterIn.DEFAULT, description = "Pet object that needs to be added to the store" ,required=true) Pet body

,
@Parameter(in = ParameterIn.HEADER, description = "status" ,required=true)@HeaderParam("token") String token

,@Parameter(in = ParameterIn.QUERY, description = "type of food",required=true) @QueryParam("petType") String petType
,@Parameter(in = ParameterIn.QUERY, description = "status",required=true) @QueryParam("status") String status
,@Parameter(in = ParameterIn.PATH, description = "ID of pet to return",required=true) @PathParam("petId") Long petId
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.feedPet(body,token,petType,status,petId,securityContext);
    }
    @GET
    
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Finds Pets by status", description = "Multiple status values can be provided with comma separated strings", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        
        @ApiResponse(responseCode = "400", description = "Invalid status value") })
    public Response findPetsByStatus(@Parameter(in = ParameterIn.QUERY, description = "Status values that need to be considered for filter",required=true, schema=@Schema(allowableValues={ "available", "pending", "sold" })
) @QueryParam("status") List<String> status
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findPetsByStatus(status,securityContext);
    }
    @GET
    
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Finds Pets by tags", description = "Muliple tags can be provided with comma separated strings. Use\\ \\ tag1, tag2, tag3 for testing.", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        
        @ApiResponse(responseCode = "400", description = "Invalid tag value") })
    public Response findPetsByTags(@Parameter(in = ParameterIn.QUERY, description = "Tags to filter by",required=true) @QueryParam("tags") List<String> tags
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findPetsByTags(tags,securityContext);
    }
    @GET
    
    
    @Produces({ "application/json" })
    @Operation(summary = "get Parrots", description = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Object.class)))) })
    public Response getParrots(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getParrots(securityContext);
    }
    @GET
    
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Find pet by ID", description = "Returns a single pet", security = {
        @SecurityRequirement(name = "api_key")    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Pet.class))),
        
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    public Response getPetById(@Parameter(in = ParameterIn.PATH, description = "ID of pet to return",required=true) @PathParam("petId") Long petId
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getPetById(petId,securityContext);
    }
    @PUT
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "update parrots", description = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input"),
        
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = InlineResponse200.class))) })
    public Response updateParrots(@Parameter(in = ParameterIn.DEFAULT, description = "" ) Body1 body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updateParrots(body,securityContext);
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
    public Response updatePet(@Parameter(in = ParameterIn.DEFAULT, description = "Pet object that needs to be added to the store" ,required=true) Pet body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updatePet(body,securityContext);
    }
    @POST
    
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @Operation(summary = "Updates a pet in the store with form data", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
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
    
    @Consumes({ "application/octet-stream" })
    @Produces({ "application/json" })
    @Operation(summary = "uploads an image", description = "", security = {
        @SecurityRequirement(name = "petstore_auth", scopes = {
            ""        })    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = ModelApiResponse.class))) })
    public Response uploadFile(@Parameter(in = ParameterIn.PATH, description = "ID of pet to update",required=true) @PathParam("petId") Long petId
,@Parameter(in = ParameterIn.DEFAULT, description = "" ) Object body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.uploadFile(petId,bodyInputStream, bodyDetail,securityContext);
    }
}
