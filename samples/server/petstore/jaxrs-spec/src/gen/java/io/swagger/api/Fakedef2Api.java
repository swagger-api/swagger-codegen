package io.swagger.api;

import io.swagger.model.Definition2;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;
import javax.validation.constraints.*;

@Path("/fakedef2")

@Api(description = "the fakedef2 API")




public class Fakedef2Api  {

    @GET
    
    
    @Produces({ "application/json", "text/json", "application/xml", "text/xml" })
    @ApiOperation(value = "Definition 2", notes = "", response = Definition2.class, responseContainer = "List", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Definition 2", response = Definition2.class, responseContainer = "List"),
        @ApiResponse(code = 404, message = "Not found", response = Definition2.class, responseContainer = "List") })
    public Response definition2() {
        return Response.ok().entity("magic!").build();
    }
}

