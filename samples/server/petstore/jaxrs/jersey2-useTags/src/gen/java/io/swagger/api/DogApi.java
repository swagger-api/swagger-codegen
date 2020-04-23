package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.DogApiService;
import io.swagger.api.factories.DogApiServiceFactory;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import io.swagger.model.Dog;

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

@Path("/dog")



public class DogApi  {
   private final DogApiService delegate;

   public DogApi(@Context ServletConfig servletContext) {
      DogApiService delegate = null;

      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("DogApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (DogApiService) Class.forName(implClass).newInstance();
            } catch (Exception e) {
               throw new RuntimeException(e);
            }
         } 
      }

      if (delegate == null) {
         delegate = DogApiServiceFactory.getDogApi();
      }

      this.delegate = delegate;
   }

    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Add a new dog to the store", description = "", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public Response addDog(@Parameter(in = ParameterIn.DEFAULT, description = "Dog object that needs to be added to the store" ,required=true) Dog body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.addDog(body,securityContext);
    }
    @DELETE
    @Path("/{dogId}")
    
    
    @Operation(summary = "Deletes a dog", description = "", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid dog value") })
    public Response deleteDog(@Parameter(in = ParameterIn.PATH, description = "Dog id to delete",required=true) @PathParam("dogId") Long dogId
,
@Parameter(in = ParameterIn.HEADER, description = "" )@HeaderParam("api_key") String apiKey

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deleteDog(dogId,apiKey,securityContext);
    }
    @GET
    @Path("/{dogId}")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find dog by ID", description = "Returns a single dog", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Dog.class))),
        
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    public Response getDogById(@Parameter(in = ParameterIn.PATH, description = "ID of dog to return",required=true) @PathParam("dogId") Long dogId
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getDogById(dogId,securityContext);
    }
    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Update an existing dog", description = "", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        
        @ApiResponse(responseCode = "404", description = "Animal not found"),
        
        @ApiResponse(responseCode = "405", description = "Validation exception") })
    public Response updateDog(@Parameter(in = ParameterIn.DEFAULT, description = "Dog object that needs to be added." ,required=true) Dog body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updateDog(body,securityContext);
    }
    @POST
    @Path("/{dogId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @Operation(summary = "Updates a dog", description = "", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public Response updateDogWithForm(@Parameter(in = ParameterIn.PATH, description = "ID of dog that needs to be updated",required=true) @PathParam("dogId") Long dogId
,@Parameter(description = "")  @FormParam("name")  String name
,@Parameter(description = "")  @FormParam("status")  String status
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updateDogWithForm(dogId,name,status,securityContext);
    }
}
