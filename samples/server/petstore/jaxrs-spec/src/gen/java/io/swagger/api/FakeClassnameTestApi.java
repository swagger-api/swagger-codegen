package io.swagger.api;

import io.swagger.model.Client;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import javax.validation.Valid;
import javax.ws.rs.core.Response;

@Path("/fake_classname_test")

@javax.annotation.Generated(value = "io.swagger.codegen.v3.generators.java.JavaJAXRSSpecServerCodegen", date = "2019-12-03T16:48:38.057+01:00[Europe/Paris]")
public class FakeClassnameTestApi {

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "To test class name in snake case", description = "", security = {
        @SecurityRequirement(name = "api_key_query")    }, tags={ "fake_classname_tags 123#$%^" })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Client.class)))
    })
    public Response testClassname(@Valid Client body) {
        return Response.ok().entity("magic!").build();
    }}
