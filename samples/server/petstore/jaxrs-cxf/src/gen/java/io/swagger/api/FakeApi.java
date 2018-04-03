package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Client;
import io.swagger.model.OuterComposite;


import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ApiResponse;
import io.swagger.jaxrs.PATCH;

import javax.validation.constraints.*;
import javax.validation.Valid;



/**
 * Swagger Petstore
 *
 
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 
 */

@Path("/")
@Api(value = "/", description = "")

public interface FakeApi  {



    
    @POST
    @Path("/fake/outer/boolean")

    @Consumes({ "*/*" })


    @Produces({ "*/*" })

    @ApiOperation(value = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output boolean", response = Boolean.class) })
    public Boolean fakeOuterBooleanSerialize(@Valid Boolean body);


    
    @POST
    @Path("/fake/outer/composite")

    @Consumes({ "*/*" })


    @Produces({ "*/*" })

    @ApiOperation(value = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output composite", response = OuterComposite.class) })
    public OuterComposite fakeOuterCompositeSerialize(@Valid OuterComposite outercomposite);


    
    @POST
    @Path("/fake/outer/number")

    @Consumes({ "*/*" })


    @Produces({ "*/*" })

    @ApiOperation(value = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output number", response = BigDecimal.class) })
    public BigDecimal fakeOuterNumberSerialize(@Valid BigDecimal body);


    
    @POST
    @Path("/fake/outer/string")

    @Consumes({ "*/*" })


    @Produces({ "*/*" })

    @ApiOperation(value = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output string", response = String.class) })
    public String fakeOuterStringSerialize(@Valid String body);


    
    /**
     * To test \&quot;client\&quot; model
     *
     
     * To test \&quot;client\&quot; model
     *
     
     */
    
    @PATCH
    @Path("/fake")

    @Consumes({ "application/json" })


    @Produces({ "application/json" })

    @ApiOperation(value = "To test \"client\" model", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    public Client testClientModel(@Valid Client client);


    
    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     
     */
    
    @POST
    @Path("/fake")

    @Consumes({ "application/xml; charset=utf-8", "application/json; charset=utf-8" })


    @ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied"),
        @ApiResponse(code = 404, message = "User not found") })
    public void testEndpointParameters(@Valid Object body);


    
    /**
     * To test enum parameters
     *
     
     * To test enum parameters
     *
     
     */
    
    @GET
    @Path("/fake")

    @Consumes({ "*/*" })


    @ApiOperation(value = "To test enum parameters", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid request"),
        @ApiResponse(code = 404, message = "Not found") })
    public void testEnumParameters(@Valid Object body, @HeaderParam("enum_header_string_array") List<String> enumHeaderStringArray, @HeaderParam("enum_header_string") String enumHeaderString, @QueryParam("enum_query_string_array") List<String> enumQueryStringArray, @QueryParam("enum_query_string") String enumQueryString, @QueryParam("enum_query_integer") Integer enumQueryInteger);


    
    /**
     * test inline additionalProperties
     *
     
     */
    
    @POST
    @Path("/fake/inline-additionalProperties")

    @Consumes({ "application/json" })


    @ApiOperation(value = "test inline additionalProperties", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation") })
    public void testInlineAdditionalProperties(@Valid Map<String, String> body);


    
    /**
     * test json serialization of form data
     *
     
     */
    
    @GET
    @Path("/fake/jsonFormData")

    @Consumes({ "application/json" })


    @ApiOperation(value = "test json serialization of form data", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation") })
    public void testJsonFormData(@Valid Object body);

}


