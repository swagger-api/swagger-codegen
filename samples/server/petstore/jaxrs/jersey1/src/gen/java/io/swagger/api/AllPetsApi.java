package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.AllPetsApiService;
import io.swagger.api.factories.AllPetsApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import com.sun.jersey.multipart.FormDataParam;
import javax.validation.constraints.*;

import io.swagger.model.AllPetsResponse;

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


@Path("/allPets")


@io.swagger.annotations.Api(description = "the allPets API")
public class AllPetsApi  {
   private final AllPetsApiService delegate = AllPetsApiServiceFactory.getAllPetsApi();

    @GET
    
    
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "", response = AllPetsResponse.class, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "a single random pet", response = AllPetsResponse.class) })
    public Response getAllPets(
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getAllPets(securityContext);
    }
}
