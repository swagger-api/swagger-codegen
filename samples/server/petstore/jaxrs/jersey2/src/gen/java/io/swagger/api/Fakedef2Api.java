package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.Fakedef2ApiService;
import io.swagger.api.factories.Fakedef2ApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import io.swagger.model.Definition2;

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

@Path("/fakedef2")


@io.swagger.annotations.Api(description = "the fakedef2 API")

public class Fakedef2Api  {
   private final Fakedef2ApiService delegate = Fakedef2ApiServiceFactory.getFakedef2Api();

    @GET
    
    
    @Produces({ "application/json", "text/json", "application/xml", "text/xml" })
    @io.swagger.annotations.ApiOperation(value = "Definition 2", notes = "", response = Definition2.class, responseContainer = "List", tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Definition 2", response = Definition2.class, responseContainer = "List"),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "Not found", response = Definition2.class, responseContainer = "List") })
    public Response definition2(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.definition2(securityContext);
    }
}
