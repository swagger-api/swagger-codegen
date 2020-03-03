package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.AnimalApiService;
import io.swagger.api.factories.AnimalApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import com.sun.jersey.multipart.FormDataParam;
import javax.validation.constraints.*;

import io.swagger.model.Animal;

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

@Path("/animal")



@io.swagger.annotations.Api(description = "the Animal API")
public class AnimalApi  {
   private final AnimalApiService delegate = AnimalApiServiceFactory.getAnimalApi();

    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @io.swagger.annotations.ApiOperation(value = "Add a new animal to the store", notes = "", response = Void.class, tags={ "animal" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    public Response addAnimal(
                        @ApiParam(value = "Animal object that needs to be added to the store" ,required=true) Animal body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.addAnimal(body,securityContext);
    }
    @DELETE
    @Path("/{animalId}")
    
    
    @io.swagger.annotations.ApiOperation(value = "Deletes a animal", notes = "", response = Void.class, tags={ "animal" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid animal value", response = Void.class) })
    public Response deleteAnimal(
                @ApiParam(value = "Animal id to delete",required=true) @PathParam("animalId") Long animalId        
,
                        @ApiParam(value = "" )@HeaderParam("api_key") String apiKey
        
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deleteAnimal(animalId,apiKey,securityContext);
    }
    @GET
    @Path("/{animalId}")
    
    @Produces({ "application/xml", "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Find animal by ID", notes = "Returns a single animal", response = Animal.class, tags={ "animal" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Animal.class),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "Pet not found", response = Void.class) })
    public Response getAnimalById(
                @ApiParam(value = "ID of pet to return",required=true) @PathParam("animalId") Long animalId        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getAnimalById(animalId,securityContext);
    }
    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    
    @io.swagger.annotations.ApiOperation(value = "Update an existing animal", notes = "", response = Void.class, tags={ "animal" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "Animal not found", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 405, message = "Validation exception", response = Void.class) })
    public Response updateAnimal(
                        @ApiParam(value = "Animal object that needs to be added." ,required=true) Animal body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updateAnimal(body,securityContext);
    }
    @POST
    @Path("/{animalId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @io.swagger.annotations.ApiOperation(value = "Updates a animal", notes = "", response = Void.class, tags={ "animal" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    public Response updateAnimalWithForm(
                @ApiParam(value = "ID of animal that needs to be updated",required=true) @PathParam("animalId") Long animalId        
,
                        @ApiParam(value = "")  @FormParam("name")  String name
,
                        @ApiParam(value = "")  @FormParam("status")  String status
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updateAnimalWithForm(animalId,name,status,securityContext);
    }
}
