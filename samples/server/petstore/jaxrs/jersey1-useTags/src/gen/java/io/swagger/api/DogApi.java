package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.DogApiService;
import io.swagger.api.factories.DogApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import com.sun.jersey.multipart.FormDataParam;
import javax.validation.constraints.*;

import io.swagger.model.Dog;

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

@Path("/dog")



@io.swagger.annotations.Api(description = "the Dog API")
public class DogApi  {
   private final DogApiService delegate = DogApiServiceFactory.getDogApi();

    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @io.swagger.annotations.ApiOperation(value = "Add a new dog to the store", notes = "", response = Void.class, tags={ "dog" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    public Response addDog(
                        @ApiParam(value = "Dog object that needs to be added to the store" ,required=true) Dog body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.addDog(body,securityContext);
    }
    @DELETE
    @Path("/{dogId}")
    
    
    @io.swagger.annotations.ApiOperation(value = "Deletes a dog", notes = "", response = Void.class, tags={ "dog" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid dog value", response = Void.class) })
    public Response deleteDog(
                @ApiParam(value = "Dog id to delete",required=true) @PathParam("dogId") Long dogId        
,
                        @ApiParam(value = "" )@HeaderParam("api_key") String apiKey
        
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deleteDog(dogId,apiKey,securityContext);
    }
    @GET
    @Path("/{dogId}")
    
    @Produces({ "application/xml", "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Find dog by ID", notes = "Returns a single dog", response = Dog.class, tags={ "dog" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Dog.class),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "Pet not found", response = Void.class) })
    public Response getDogById(
                @ApiParam(value = "ID of dog to return",required=true) @PathParam("dogId") Long dogId        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getDogById(dogId,securityContext);
    }
    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    
    @io.swagger.annotations.ApiOperation(value = "Update an existing dog", notes = "", response = Void.class, tags={ "dog" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "Animal not found", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 405, message = "Validation exception", response = Void.class) })
    public Response updateDog(
                        @ApiParam(value = "Dog object that needs to be added." ,required=true) Dog body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updateDog(body,securityContext);
    }
    @POST
    @Path("/{dogId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @io.swagger.annotations.ApiOperation(value = "Updates a dog", notes = "", response = Void.class, tags={ "dog" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    public Response updateDogWithForm(
                @ApiParam(value = "ID of dog that needs to be updated",required=true) @PathParam("dogId") Long dogId        
,
                        @ApiParam(value = "")  @FormParam("name")  String name
,
                        @ApiParam(value = "")  @FormParam("status")  String status
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updateDogWithForm(dogId,name,status,securityContext);
    }
}
