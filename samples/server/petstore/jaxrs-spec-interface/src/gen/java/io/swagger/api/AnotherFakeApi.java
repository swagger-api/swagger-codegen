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

@javax.annotation.Generated(value = "io.swagger.codegen.languages.java.JavaJAXRSSpecServerCodegen", date = "2018-04-04T20:14:19.902+02:00[Europe/Zurich]")
public interface AnotherFakeApi {



    @PATCH
    @Path("/dummy")
    @ApiOperation(value = "To test special tags", notes = "To test special tags", tags={ "$another-fake?" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    Client testSpecialTags(@Valid Client client);

}
