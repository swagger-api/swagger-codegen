package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.FakeApiService;
import io.swagger.api.factories.FakeApiServiceFactory;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import java.math.BigDecimal;
import io.swagger.model.Client;
import io.swagger.model.OuterComposite;

import java.util.Map;
import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import org.glassfish.jersey.media.multipart.FormDataParam;

import javax.servlet.ServletConfig;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.validation.constraints.*;

@Path("/fake")


public class FakeApi  {
   private final FakeApiService delegate;

   public FakeApi(@Context ServletConfig servletContext) {
      FakeApiService delegate = null;

      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("FakeApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (FakeApiService) Class.forName(implClass).newInstance();
            } catch (Exception e) {
               throw new RuntimeException(e);
            }
         } 
      }

      if (delegate == null) {
         delegate = FakeApiServiceFactory.getFakeApi();
      }

      this.delegate = delegate;
   }

    @POST
    @Path("/outer/boolean")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "Test serialization of outer boolean types", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output boolean", content = @Content(schema = @Schema(implementation = Boolean.class))) })
    public Response fakeOuterBooleanSerialize(@Parameter(description = "Input boolean as post body" ) Boolean body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeOuterBooleanSerialize(body,securityContext);
    }
    @POST
    @Path("/outer/composite")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "Test serialization of object with outer number type", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output composite", content = @Content(schema = @Schema(implementation = OuterComposite.class))) })
    public Response fakeOuterCompositeSerialize(@Parameter(description = "Input composite as post body" ) OuterComposite body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeOuterCompositeSerialize(body,securityContext);
    }
    @POST
    @Path("/outer/number")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "Test serialization of outer number types", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output number", content = @Content(schema = @Schema(implementation = BigDecimal.class))) })
    public Response fakeOuterNumberSerialize(@Parameter(description = "Input number as post body" ) BigDecimal body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeOuterNumberSerialize(body,securityContext);
    }
    @POST
    @Path("/outer/string")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "Test serialization of outer string types", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output string", content = @Content(schema = @Schema(implementation = String.class))) })
    public Response fakeOuterStringSerialize(@Parameter(description = "Input string as post body" ) String body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeOuterStringSerialize(body,securityContext);
    }
    @PATCH
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "To test \"client\" model", description = "To test \"client\" model", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Client.class))) })
    public Response testClientModel(@Parameter(description = "client model" ,required=true) Client body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testClientModel(body,securityContext);
    }
    @POST
    
    @Consumes({ "application/xml; charset=utf-8", "application/json; charset=utf-8" })
    
    @Operation(summary = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", description = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        
        @ApiResponse(responseCode = "404", description = "User not found") })
    public Response testEndpointParameters(@Parameter(description = "" ,required=true) Object body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testEndpointParameters(body,securityContext);
    }
    @GET
    
    @Consumes({ "*/*" })
    
    @Operation(summary = "To test enum parameters", description = "To test enum parameters", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid request"),
        
        @ApiResponse(responseCode = "404", description = "Not found") })
    public Response testEnumParameters(@Parameter(description = "" ) Object body

,
@Parameter(description = "Header parameter enum test (string array)" , schema=@Schema(allowableValues={ ">", "$" })
)@HeaderParam("enum_header_string_array") List<String> enumHeaderStringArray

,
@Parameter(description = "Header parameter enum test (string)" )@HeaderParam("enum_header_string") String enumHeaderString

,@Parameter(description = "Query parameter enum test (string array)", schema=@Schema(allowableValues={ ">", "$" })
) @QueryParam("enum_query_string_array") List<String> enumQueryStringArray
,@Parameter(description = "Query parameter enum test (string)") @QueryParam("enum_query_string") String enumQueryString
,@Parameter(description = "Query parameter enum test (double)") @QueryParam("enum_query_integer") Integer enumQueryInteger
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testEnumParameters(body,enumHeaderStringArray,enumHeaderString,enumQueryStringArray,enumQueryString,enumQueryInteger,securityContext);
    }
    @POST
    @Path("/inline-additionalProperties")
    @Consumes({ "application/json" })
    
    @Operation(summary = "test inline additionalProperties", description = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public Response testInlineAdditionalProperties(@Parameter(description = "request body" ,required=true) Map<String, String> body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testInlineAdditionalProperties(body,securityContext);
    }
    @GET
    @Path("/jsonFormData")
    @Consumes({ "application/json" })
    
    @Operation(summary = "test json serialization of form data", description = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public Response testJsonFormData(@Parameter(description = "" ,required=true) Object body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testJsonFormData(body,securityContext);
    }
}
