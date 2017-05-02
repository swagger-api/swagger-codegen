package io.swagger.api;

import io.swagger.model.Definition1;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;
import javax.validation.constraints.*;

@Path("/fakedef1")

@Api(description = "the fakedef1 API")




public class Fakedef1Api  {

    @GET
    
    
    @Produces({ "application/json", "text/json", "application/xml", "text/xml" })
    @ApiOperation(value = "Definition 1", notes = "", response = Definition1.class, responseContainer = "List", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Definition 1", response = Definition1.class, responseContainer = "List"),
        @ApiResponse(code = 404, message = "Not found", response = Definition1.class, responseContainer = "List") })
    public Response definition1() {
        return Response.ok().entity("magic!").build();
    }

    @POST
    
    
    
    @ApiOperation(value = "", notes = "submitting JSON", response = void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "received Raw JSON", response = void.class) })
    public Response postJSON(Object body) {
        return Response.ok().entity("magic!").build();
    }
}

