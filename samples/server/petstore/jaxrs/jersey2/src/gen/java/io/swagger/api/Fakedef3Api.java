package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.Fakedef3ApiService;
import io.swagger.api.factories.Fakedef3ApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import io.swagger.model.Definition3;

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

@Path("/fakedef3")


@io.swagger.annotations.Api(description = "the fakedef3 API")

public class Fakedef3Api  {
   private final Fakedef3ApiService delegate = Fakedef3ApiServiceFactory.getFakedef3Api();

    @GET
    
    
    @Produces({ "application/json", "text/json", "application/xml", "text/xml" })
    @io.swagger.annotations.ApiOperation(value = "Definition 3", notes = "", response = Definition3.class, responseContainer = "List", tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Definition 3", response = Definition3.class, responseContainer = "List"),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "Not found", response = Definition3.class, responseContainer = "List") })
    public Response definition3(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.definition3(securityContext);
    }
}
