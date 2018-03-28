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

@javax.annotation.Generated(value = "io.swagger.codegen.languages.java.JavaJAXRSSpecServerCodegen", date = "2018-03-15T06:28:22.549+01:00[Europe/Zurich]")
public interface FakeApi {



    @POST
    @Path("/outer/boolean")
    @ApiOperation(value = "", notes = "Test serialization of outer boolean types", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output boolean", response = Boolean.class) })
    Boolean fakeOuterBooleanSerialize(@Valid Boolean body);


    @POST
    @Path("/outer/composite")
    @ApiOperation(value = "", notes = "Test serialization of object with outer number type", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output composite", response = OuterComposite.class) })
    OuterComposite fakeOuterCompositeSerialize(@Valid OuterComposite outercomposite);


    @POST
    @Path("/outer/number")
    @ApiOperation(value = "", notes = "Test serialization of outer number types", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output number", response = BigDecimal.class) })
    BigDecimal fakeOuterNumberSerialize(@Valid BigDecimal body);


    @POST
    @Path("/outer/string")
    @ApiOperation(value = "", notes = "Test serialization of outer string types", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output string", response = String.class) })
    String fakeOuterStringSerialize(@Valid String body);


    @PATCH
    @ApiOperation(value = "To test \"client\" model", notes = "To test \"client\" model", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    Client testClientModel(@Valid Client client);


    @POST
    @ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", notes = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    void testEndpointParameters(@Valid Object body);


    @GET
    @ApiOperation(value = "To test enum parameters", notes = "To test enum parameters", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid request", response = Void.class),
        @ApiResponse(code = 404, message = "Not found", response = Void.class) })
    void testEnumParameters(@Valid Object body,@HeaderParam("enum_header_string_array")   @ApiParam("Header parameter enum test (string array)") List<String> enumHeaderStringArray,@HeaderParam("enum_header_string")   @ApiParam("Header parameter enum test (string)") String enumHeaderString,@QueryParam("enum_query_string_array")   @ApiParam("Query parameter enum test (string array)")  List<String> enumQueryStringArray,@QueryParam("enum_query_string")   @ApiParam("Query parameter enum test (string)")  String enumQueryString,@QueryParam("enum_query_integer")   @ApiParam("Query parameter enum test (double)")  Integer enumQueryInteger);


    @POST
    @Path("/inline-additionalProperties")
    @ApiOperation(value = "test inline additionalProperties", notes = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    void testInlineAdditionalProperties(@Valid Map<String, String> body);


    @GET
    @Path("/jsonFormData")
    @ApiOperation(value = "test json serialization of form data", notes = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    void testJsonFormData(@Valid Object body);

}
