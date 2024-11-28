package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.PetApiService;
import io.swagger.api.factories.PetApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import com.sun.jersey.multipart.FormDataParam;
import javax.validation.constraints.*;

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

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;


@Path("/Pet")


@io.swagger.annotations.Api(description = "the Pet API")
public class PetApi  {
   private final PetApiService delegate = PetApiServiceFactory.getPetApi();

    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Add a new parrow to the store", notes = "", response = InlineResponse2001.class, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = InlineResponse2001.class) })
    public Response addParrot(
                        @ApiParam(value = "" ) Body2 body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.addParrot(body,securityContext);
    }
    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @io.swagger.annotations.ApiOperation(value = "Add a new pet to the store", notes = "", response = Void.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "", description = "")        })    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    public Response addPet(
                        @ApiParam(value = "Pet object that needs to be added to the store" ,required=true) Pet body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.addPet(body,securityContext);
    }
    @DELETE
    
    
    
    @io.swagger.annotations.ApiOperation(value = "Deletes a pet", notes = "", response = Void.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "", description = "")        })    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "Pet not found", response = Void.class) })
    public Response deletePet(
                @ApiParam(value = "Pet id to delete",required=true) @PathParam("petId") Long petId        
,
                        @ApiParam(value = "" )@HeaderParam("api_key") String apiKey
        
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deletePet(petId,apiKey,securityContext);
    }
    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @io.swagger.annotations.ApiOperation(value = "Find pet by ID", notes = "schedule pet feeding", response = Void.class, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    public Response feedPet(
                        @ApiParam(value = "Pet object that needs to be added to the store" ,required=true) Pet body
        
,
                        @ApiParam(value = "status" ,required=true)@HeaderParam("token") String token
        
        
,
                @ApiParam(value = "type of food",required=true) @QueryParam("petType") String petType        
,
                @ApiParam(value = "status",required=true) @QueryParam("status") String status        
,
                @ApiParam(value = "ID of pet to return",required=true) @PathParam("petId") Long petId        
,
                        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.feedPet(body,token,petType,status,petId,sessionId,securityContext);
    }
    @GET
    
    
    @Produces({ "application/json", "application/xml" })
    @io.swagger.annotations.ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma separated strings", response = Pet.class, responseContainer = "List", authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "", description = "")        })    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid status value", response = Void.class) })
    public Response findPetsByStatus(
                @ApiParam(value = "Status values that need to be considered for filter",required=true, allowableValues="available, pending, sold"
) @QueryParam("status") List<String> status        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findPetsByStatus(status,securityContext);
    }
    @GET
    
    
    @Produces({ "application/json", "application/xml" })
    @io.swagger.annotations.ApiOperation(value = "Finds Pets by tags", notes = "Muliple tags can be provided with comma separated strings. Use\\ \\ tag1, tag2, tag3 for testing.", response = Pet.class, responseContainer = "List", authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "", description = "")        })    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid tag value", response = Void.class) })
    public Response findPetsByTags(
                @ApiParam(value = "Tags to filter by",required=true) @QueryParam("tags") List<String> tags        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findPetsByTags(tags,securityContext);
    }
    @GET
    
    
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "get Parrots", notes = "", response = Object.class, responseContainer = "List", tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Object.class, responseContainer = "List") })
    public Response getParrots(
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getParrots(securityContext);
    }
    @GET
    
    
    @Produces({ "application/json", "application/xml" })
    @io.swagger.annotations.ApiOperation(value = "Find pet by ID", notes = "Returns a single pet", response = Pet.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "api_key")    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Pet.class),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "Pet not found", response = Void.class) })
    public Response getPetById(
                @ApiParam(value = "ID of pet to return",required=true) @PathParam("petId") Long petId        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getPetById(petId,securityContext);
    }
    @PUT
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "update parrots", notes = "", response = InlineResponse200.class, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = InlineResponse200.class) })
    public Response updateParrots(
                        @ApiParam(value = "" ) Body1 body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updateParrots(body,securityContext);
    }
    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    
    @io.swagger.annotations.ApiOperation(value = "Update an existing pet", notes = "", response = Void.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "", description = "")        })    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "Pet not found", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 405, message = "Validation exception", response = Void.class) })
    public Response updatePet(
                        @ApiParam(value = "Pet object that needs to be added to the store" ,required=true) Pet body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updatePet(body,securityContext);
    }
    @POST
    
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @io.swagger.annotations.ApiOperation(value = "Updates a pet in the store with form data", notes = "", response = Void.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "", description = "")        })    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    public Response updatePetWithForm(
                @ApiParam(value = "ID of pet that needs to be updated",required=true) @PathParam("petId") Long petId        
,
                        @ApiParam(value = "")  @FormParam("name")  String name
,
                        @ApiParam(value = "")  @FormParam("status")  String status
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updatePetWithForm(petId,name,status,securityContext);
    }
    @POST
    
    @Consumes({ "application/octet-stream" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "uploads an image", notes = "", response = ModelApiResponse.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "", description = "")        })    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class) })
    public Response uploadFile(
                @ApiParam(value = "ID of pet to update",required=true) @PathParam("petId") Long petId        
,
                        @ApiParam(value = "" ) Object body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.uploadFile(petId, bodyInputStream, bodyDetail,securityContext);
    }
}
