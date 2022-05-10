package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.ParrotApiService;
import io.swagger.api.factories.ParrotApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import com.sun.jersey.multipart.FormDataParam;
import javax.validation.constraints.*;

import io.swagger.model.Body1;
import io.swagger.model.Body2;
import io.swagger.model.InlineResponse200;
import io.swagger.model.InlineResponse2001;

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


@Path("/parrot")


@io.swagger.annotations.Api(description = "the parrot API")
public class ParrotApi  {
   private final ParrotApiService delegate = ParrotApiServiceFactory.getParrotApi();

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
}
