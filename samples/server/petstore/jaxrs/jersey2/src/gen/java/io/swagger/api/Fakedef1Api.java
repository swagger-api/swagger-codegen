package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.Fakedef1ApiService;
import io.swagger.api.factories.Fakedef1ApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import io.swagger.model.Definition1;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import org.glassfish.jersey.media.multipart.FormDataParam;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.validation.constraints.*;

@Path("/fakedef1")


@io.swagger.annotations.Api(description = "the fakedef1 API")

public class Fakedef1Api  {
   private final Fakedef1ApiService delegate = Fakedef1ApiServiceFactory.getFakedef1Api();

    @GET
    
    
    @Produces({ "application/json", "text/json", "application/xml", "text/xml" })
    @io.swagger.annotations.ApiOperation(value = "Definition 1", notes = "", response = Definition1.class, responseContainer = "List", tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Definition 1", response = Definition1.class, responseContainer = "List"),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "Not found", response = Definition1.class, responseContainer = "List") })
    public Response definition1(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.definition1(securityContext);
    }
    @POST
    
    
    
    @io.swagger.annotations.ApiOperation(value = "", notes = "submitting JSON", response = void.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "received Raw JSON", response = void.class) })
    public Response postJSON(@ApiParam(value = "JSON" ,required=true) Object body
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.postJSON(body,securityContext);
    }
}
