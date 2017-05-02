package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.FakeArraysApiService;
import io.swagger.api.factories.FakeArraysApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import java.util.List;

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

@Path("/fakeArrays")


@io.swagger.annotations.Api(description = "the fakeArrays API")

public class FakeArraysApi  {
   private final FakeArraysApiService delegate = FakeArraysApiServiceFactory.getFakeArraysApi();

    @GET
    
    
    
    @io.swagger.annotations.ApiOperation(value = "", notes = "submitting JSON", response = void.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = {  })
    public Response postJSON(@ApiParam(value = "Array of ints",required=true) @QueryParam("items") Long items
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.postJSON(items,securityContext);
    }
    @POST
    
    
    
    @io.swagger.annotations.ApiOperation(value = "", notes = "submitting JSON", response = void.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = {  })
    public Response postJSON_1(@ApiParam(value = "Array of ints" ,required=true) List<Long> items
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.postJSON_1(items,securityContext);
    }
}
