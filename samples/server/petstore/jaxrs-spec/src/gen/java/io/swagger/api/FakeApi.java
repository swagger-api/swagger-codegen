package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Client;
import io.swagger.model.OuterComposite;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/fake")

@javax.annotation.Generated(value = "io.swagger.codegen.v3.generators.java.JavaJAXRSSpecServerCodegen", date = "2019-02-14T23:05:13.797-05:00[America/Bogota]")
public class FakeApi {

    @POST
    @Path("/outer/boolean")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "Test serialization of outer boolean types", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output boolean", content = @Content(schema = @Schema(implementation = Boolean.class)))
    })
    public Response fakeOuterBooleanSerialize(@Valid Boolean body) {
        return Response.ok().entity("magic!").build();
    }
    @POST
    @Path("/outer/composite")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "Test serialization of object with outer number type", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output composite", content = @Content(schema = @Schema(implementation = OuterComposite.class)))
    })
    public Response fakeOuterCompositeSerialize(@Valid OuterComposite body) {
        return Response.ok().entity("magic!").build();
    }
    @POST
    @Path("/outer/number")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "Test serialization of outer number types", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output number", content = @Content(schema = @Schema(implementation = BigDecimal.class)))
    })
    public Response fakeOuterNumberSerialize(@Valid BigDecimal body) {
        return Response.ok().entity("magic!").build();
    }
    @POST
    @Path("/outer/string")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "Test serialization of outer string types", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output string", content = @Content(schema = @Schema(implementation = String.class)))
    })
    public Response fakeOuterStringSerialize(@Valid String body) {
        return Response.ok().entity("magic!").build();
    }
    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "To test \"client\" model", description = "To test \"client\" model", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Client.class)))
    })
    public Response testClientModel(@Valid Client body) {
        return Response.ok().entity("magic!").build();
    }
    @POST
    @Consumes({ "application/xml; charset=utf-8", "application/json; charset=utf-8" })
    @Operation(summary = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", description = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", security = {
        @SecurityRequirement(name = "http_basic_test")    }, tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        @ApiResponse(responseCode = "404", description = "User not found")
    })
    public Response testEndpointParameters(@Valid Object body) {
        return Response.ok().entity("magic!").build();
    }
    @GET
    @Consumes({ "*/*" })
    @Operation(summary = "To test enum parameters", description = "To test enum parameters", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid request"),
        @ApiResponse(responseCode = "404", description = "Not found")
    })
    public Response testEnumParameters(@Valid Object body,  @HeaderParam("enum_header_string_array") 

 @Parameter(description = "Header parameter enum test (string array)") List<String> enumHeaderStringArray
,  @HeaderParam("enum_header_string") 

 @Parameter(description = "Header parameter enum test (string)") String enumHeaderString
,  @QueryParam("enum_query_string_array") 

 @Parameter(description = "Query parameter enum test (string array)")  List<String> enumQueryStringArray
,  @QueryParam("enum_query_string") 

 @Parameter(description = "Query parameter enum test (string)")  String enumQueryString
,  @QueryParam("enum_query_integer") 

 @Parameter(description = "Query parameter enum test (double)")  Integer enumQueryInteger
) {
        return Response.ok().entity("magic!").build();
    }
    @POST
    @Path("/inline-additionalProperties")
    @Consumes({ "application/json" })
    @Operation(summary = "test inline additionalProperties", description = "", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response testInlineAdditionalProperties(@Valid Map<String, String> body) {
        return Response.ok().entity("magic!").build();
    }
    @GET
    @Path("/jsonFormData")
    @Consumes({ "application/json" })
    @Operation(summary = "test json serialization of form data", description = "", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response testJsonFormData(@Valid Object body) {
        return Response.ok().entity("magic!").build();
    }}
