package io.swagger.api;

import io.swagger.model.Client;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;

import javax.validation.Valid;
import javax.ws.rs.core.Response;

@Path("/another-fake")

@javax.annotation.Generated(value = "io.swagger.codegen.v3.generators.java.JavaJAXRSSpecServerCodegen", date = "2019-12-03T16:48:38.057+01:00[Europe/Paris]")
public class AnotherFakeApi {

    @PATCH
    @Path("/dummy")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "To test special tags", description = "To test special tags", tags={ "$another-fake?" })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Client.class)))
    })
    public Response testSpecialTags(@Valid Client body) {
        return Response.ok().entity("magic!").build();
    }}
