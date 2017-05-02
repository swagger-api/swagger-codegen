package io.swagger.api;

import io.swagger.model.Definition1;
import io.swagger.model.Definition2;
import io.swagger.model.Definition3;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;
import javax.validation.constraints.*;

@Path("/api")

@Api(description = "the api API")




public class ApiApi  {

    @GET
    @Path("/fakedef1")
    
    @Produces({ "application/json", "text/json", "application/xml", "text/xml" })
    @ApiOperation(value = "Definition 1", notes = "", response = Definition1.class, responseContainer = "List", tags={ "Catalog",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Definition 1", response = Definition1.class, responseContainer = "List"),
        @ApiResponse(code = 404, message = "Not found", response = Definition1.class, responseContainer = "List") })
    public Response definition1() {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/fakedef2")
    
    @Produces({ "application/json", "text/json", "application/xml", "text/xml" })
    @ApiOperation(value = "Definition 2", notes = "", response = Definition2.class, responseContainer = "List", tags={ "Catalog",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Definition 2", response = Definition2.class, responseContainer = "List"),
        @ApiResponse(code = 404, message = "Not found", response = Definition2.class, responseContainer = "List") })
    public Response definition2() {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/fakedef3")
    
    @Produces({ "application/json", "text/json", "application/xml", "text/xml" })
    @ApiOperation(value = "Definition 3", notes = "", response = Definition3.class, responseContainer = "List", tags={ "Catalog" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Definition 3", response = Definition3.class, responseContainer = "List"),
        @ApiResponse(code = 404, message = "Not found", response = Definition3.class, responseContainer = "List") })
    public Response definition3() {
        return Response.ok().entity("magic!").build();
    }
}

