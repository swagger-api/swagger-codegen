package io.swagger.api;

import io.swagger.model.Client;


import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/another-fake")
@Api(description = "the another-fake API")

@javax.annotation.Generated(value = "io.swagger.codegen.languages.java.JavaJAXRSSpecServerCodegen", date = "2018-03-15T06:28:26.585+01:00[Europe/Zurich]")
public class AnotherFakeApi {



    @PATCH
    @Path("/dummy")
    @ApiOperation(value = "To test special tags", notes = "To test special tags", response = Client.class, tags={ "$another-fake?" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class)
    })
    public Response testSpecialTags(@Valid Client client) {
        return Response.ok().entity("magic!").build();
    }

}
