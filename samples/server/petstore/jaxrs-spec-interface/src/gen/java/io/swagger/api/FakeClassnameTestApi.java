package io.swagger.api;

import io.swagger.model.Client;


import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/fake_classname_test")
@Api(description = "the fake_classname_test API")

@javax.annotation.Generated(value = "io.swagger.codegen.languages.java.JavaJAXRSSpecServerCodegen", date = "2018-03-15T06:28:22.549+01:00[Europe/Zurich]")
public interface FakeClassnameTestApi {



    @PATCH
    @ApiOperation(value = "To test class name in snake case", notes = "", tags={ "fake_classname_tags 123#$%^" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    Client testClassname(@Valid Client client);

}
