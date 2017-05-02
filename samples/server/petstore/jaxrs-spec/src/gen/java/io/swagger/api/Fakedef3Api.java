package io.swagger.api;

import io.swagger.model.Definition3;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;
import javax.validation.constraints.*;

@Path("/fakedef3")

@Api(description = "the fakedef3 API")




public class Fakedef3Api  {

    @GET
    
    
    @Produces({ "application/json", "text/json", "application/xml", "text/xml" })
    @ApiOperation(value = "Definition 3", notes = "", response = Definition3.class, responseContainer = "List", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Definition 3", response = Definition3.class, responseContainer = "List"),
        @ApiResponse(code = 404, message = "Not found", response = Definition3.class, responseContainer = "List") })
    public Response definition3() {
        return Response.ok().entity("magic!").build();
    }
}

