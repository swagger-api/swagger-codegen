package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Client;
import io.swagger.model.OuterComposite;


import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/fake")
@Api(description = "the fake API")

@javax.annotation.Generated(value = "io.swagger.codegen.languages.java.JavaJAXRSSpecServerCodegen", date = "2018-04-04T20:14:22.579+02:00[Europe/Zurich]")
public class FakeApi {



    @POST
    @Path("/outer/boolean")
    @ApiOperation(value = "", notes = "Test serialization of outer boolean types", response = Boolean.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output boolean", response = Boolean.class)
    })
    public Response fakeOuterBooleanSerialize(@Valid Boolean body) {
        return Response.ok().entity("magic!").build();
    }


    @POST
    @Path("/outer/composite")
    @ApiOperation(value = "", notes = "Test serialization of object with outer number type", response = OuterComposite.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output composite", response = OuterComposite.class)
    })
    public Response fakeOuterCompositeSerialize(@Valid OuterComposite outercomposite) {
        return Response.ok().entity("magic!").build();
    }


    @POST
    @Path("/outer/number")
    @ApiOperation(value = "", notes = "Test serialization of outer number types", response = BigDecimal.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output number", response = BigDecimal.class)
    })
    public Response fakeOuterNumberSerialize(@Valid BigDecimal body) {
        return Response.ok().entity("magic!").build();
    }


    @POST
    @Path("/outer/string")
    @ApiOperation(value = "", notes = "Test serialization of outer string types", response = String.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output string", response = String.class)
    })
    public Response fakeOuterStringSerialize(@Valid String body) {
        return Response.ok().entity("magic!").build();
    }


    @PATCH
    @ApiOperation(value = "To test \"client\" model", notes = "To test \"client\" model", response = Client.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class)
    })
    public Response testClientModel(@Valid Client client) {
        return Response.ok().entity("magic!").build();
    }


    @POST
    @ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", notes = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", response = Void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class)
    })
    public Response testEndpointParameters(@Valid Object body) {
        return Response.ok().entity("magic!").build();
    }


    @GET
    @ApiOperation(value = "To test enum parameters", notes = "To test enum parameters", response = Void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid request", response = Void.class),
        @ApiResponse(code = 404, message = "Not found", response = Void.class)
    })
    public Response testEnumParameters(@Valid Object body,@HeaderParam("enum_header_string_array")   @ApiParam("Header parameter enum test (string array)") List<String> enumHeaderStringArray,@HeaderParam("enum_header_string")   @ApiParam("Header parameter enum test (string)") String enumHeaderString,@QueryParam("enum_query_string_array")   @ApiParam("Query parameter enum test (string array)")  List<String> enumQueryStringArray,@QueryParam("enum_query_string")   @ApiParam("Query parameter enum test (string)")  String enumQueryString,@QueryParam("enum_query_integer")   @ApiParam("Query parameter enum test (double)")  Integer enumQueryInteger) {
        return Response.ok().entity("magic!").build();
    }


    @POST
    @Path("/inline-additionalProperties")
    @ApiOperation(value = "test inline additionalProperties", notes = "", response = Void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class)
    })
    public Response testInlineAdditionalProperties(@Valid Map<String, String> body) {
        return Response.ok().entity("magic!").build();
    }


    @GET
    @Path("/jsonFormData")
    @ApiOperation(value = "test json serialization of form data", notes = "", response = Void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class)
    })
    public Response testJsonFormData(@Valid Object body) {
        return Response.ok().entity("magic!").build();
    }

}
