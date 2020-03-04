package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.RandomPetApiService;
import io.swagger.api.factories.RandomPetApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import com.sun.jersey.multipart.FormDataParam;
import javax.validation.constraints.*;

import io.swagger.model.SinglePetResponse;

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


@Path("/randomPet")


@io.swagger.annotations.Api(description = "the randomPet API")
public class RandomPetApi  {
   private final RandomPetApiService delegate = RandomPetApiServiceFactory.getRandomPetApi();

    @GET
    
    
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "", response = SinglePetResponse.class, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "a single random pet", response = SinglePetResponse.class) })
    public Response getRandomPet(
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getRandomPet(securityContext);
    }
}
