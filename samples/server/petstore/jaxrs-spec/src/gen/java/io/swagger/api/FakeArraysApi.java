package io.swagger.api;

import java.util.List;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;
import javax.validation.constraints.*;

@Path("/fakeArrays")

@Api(description = "the fakeArrays API")




public class FakeArraysApi  {

    @GET
    
    
    
    @ApiOperation(value = "", notes = "submitting JSON", response = void.class, tags={ "fake",  })
    @ApiResponses(value = {  })
    public Response postJSON(@QueryParam("items") @NotNull  Long items) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    
    
    
    @ApiOperation(value = "", notes = "submitting JSON", response = void.class, tags={ "fake" })
    @ApiResponses(value = {  })
    public Response postJSON_1(List<Long> items) {
        return Response.ok().entity("magic!").build();
    }
}

