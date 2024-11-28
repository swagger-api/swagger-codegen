package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.TestApiService;
import io.swagger.api.factories.TestApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import com.sun.jersey.multipart.FormDataParam;
import javax.validation.constraints.*;


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


@Path("/test")


@io.swagger.annotations.Api(description = "the test API")
public class TestApi  {
   private final TestApiService delegate = TestApiServiceFactory.getTestApi();

    @GET
    
    
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "", response = String.class, responseContainer = "List", authorizations = {
        @io.swagger.annotations.Authorization(value = "bearer")    }, tags={  })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "peticion realizada con exito", response = String.class, responseContainer = "List") })
    public Response testMethod(
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testMethod(securityContext);
    }
}
