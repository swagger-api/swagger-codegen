package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.FakeApiService;
import io.swagger.api.factories.FakeApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import com.sun.jersey.multipart.FormDataParam;
import javax.validation.constraints.*;

import java.math.BigDecimal;
import io.swagger.model.Body2;
import io.swagger.model.Body3;
import io.swagger.model.Body4;
import io.swagger.model.Body5;
import io.swagger.model.Client;
import io.swagger.model.OuterComposite;

import java.util.Map;
import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;


@Path("/fake")


@io.swagger.annotations.Api(description = "the fake API")
public class FakeApi  {
   private final FakeApiService delegate = FakeApiServiceFactory.getFakeApi();

    @POST
    @Path("/outer/boolean")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "Test serialization of outer boolean types", response = Boolean.class, tags={ "fake" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Output boolean", response = Boolean.class) })
    public Response fakeOuterBooleanSerialize(
                        @ApiParam(value = "Input boolean as post body" ) Boolean body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeOuterBooleanSerialize(body,securityContext);
    }
    @POST
    @Path("/outer/composite")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "Test serialization of object with outer number type", response = OuterComposite.class, tags={ "fake" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Output composite", response = OuterComposite.class) })
    public Response fakeOuterCompositeSerialize(
                        @ApiParam(value = "Input composite as post body" ) OuterComposite body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeOuterCompositeSerialize(body,securityContext);
    }
    @POST
    @Path("/outer/number")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "Test serialization of outer number types", response = BigDecimal.class, tags={ "fake" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Output number", response = BigDecimal.class) })
    public Response fakeOuterNumberSerialize(
                        @ApiParam(value = "Input number as post body" ) BigDecimal body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeOuterNumberSerialize(body,securityContext);
    }
    @POST
    @Path("/outer/string")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "Test serialization of outer string types", response = String.class, tags={ "fake" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Output string", response = String.class) })
    public Response fakeOuterStringSerialize(
                        @ApiParam(value = "Input string as post body" ) String body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeOuterStringSerialize(body,securityContext);
    }
    @PATCH
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "To test \"client\" model", notes = "To test \"client\" model", response = Client.class, tags={ "fake" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    public Response testClientModel(
                        @ApiParam(value = "client model" ,required=true) Client body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testClientModel(body,securityContext);
    }
    @POST
    
    @Consumes({ "application/xml; charset=utf-8", "application/json; charset=utf-8" })
    
    @io.swagger.annotations.ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", notes = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", response = Void.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "http_basic_test")    }, tags={ "fake" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "User not found", response = Void.class) })
    public Response testEndpointParameters(
                        @ApiParam(value = "" ,required=true) Body2 body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testEndpointParameters(body,securityContext);
    }
    @GET
    
    
    
    @io.swagger.annotations.ApiOperation(value = "To test enum parameters", notes = "To test enum parameters", response = Void.class, tags={ "fake" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid request", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "Not found", response = Void.class) })
    public Response testEnumParameters(
                        @ApiParam(value = "Header parameter enum test (string array)" , allowableValues=">, $"
)@HeaderParam("enum_header_string_array") List<String> enumHeaderStringArray
        
        
,
                        @ApiParam(value = "Header parameter enum test (string)" , allowableValues="_abc, -efg, (xyz)"
, defaultValue="-efg")@HeaderParam("enum_header_string") String enumHeaderString
        
        
,
                @ApiParam(value = "Query parameter enum test (string array)", allowableValues=">, $"
) @QueryParam("enum_query_string_array") List<String> enumQueryStringArray        
,
                @ApiParam(value = "Query parameter enum test (string)", allowableValues="_abc, -efg, (xyz)"
, defaultValue="-efg") @DefaultValue("-efg") @QueryParam("enum_query_string") String enumQueryString        
,
                @ApiParam(value = "Query parameter enum test (double)", allowableValues="1, -2"
) @QueryParam("enum_query_integer") Integer enumQueryInteger        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testEnumParameters(enumHeaderStringArray,enumHeaderString,enumQueryStringArray,enumQueryString,enumQueryInteger,securityContext);
    }
    @POST
    @Path("/enum/form")
    @Consumes({ "*/*" })
    
    @io.swagger.annotations.ApiOperation(value = "To test enum parameters", notes = "To test enum parameters", response = Void.class, tags={ "fake" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid request", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "Not found", response = Void.class) })
    public Response testEnumRequestBody(
                        @ApiParam(value = "" ) Body4 body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testEnumRequestBody(body,securityContext);
    }
    @POST
    @Path("/inline-additionalProperties")
    @Consumes({ "application/json" })
    
    @io.swagger.annotations.ApiOperation(value = "test inline additionalProperties", notes = "", response = Void.class, tags={ "fake" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    public Response testInlineAdditionalProperties(
                        @ApiParam(value = "request body" ,required=true) Map<String, String> body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testInlineAdditionalProperties(body,securityContext);
    }
    @POST
    @Path("/jsonFormData")
    @Consumes({ "application/json" })
    
    @io.swagger.annotations.ApiOperation(value = "test json serialization of form data", notes = "", response = Void.class, tags={ "fake" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    public Response testJsonFormData(
                        @ApiParam(value = "" ,required=true) Body5 body
        
,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testJsonFormData(body,securityContext);
    }
}
