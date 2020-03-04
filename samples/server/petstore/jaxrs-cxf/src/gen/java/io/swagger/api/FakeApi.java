package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Body2;
import io.swagger.model.Body3;
import io.swagger.model.Body4;
import io.swagger.model.Body5;
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

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
 * Swagger Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
@Path("/")
public interface FakeApi  {

    @POST
    @Path("/fake/outer/boolean")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @Operation(summary = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output boolean", content = @Content(schema = @Schema(implementation = Boolean.class))) })
    public Boolean fakeOuterBooleanSerialize(@Valid Boolean body);

    @POST
    @Path("/fake/outer/composite")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @Operation(summary = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output composite", content = @Content(schema = @Schema(implementation = OuterComposite.class))) })
    public OuterComposite fakeOuterCompositeSerialize(@Valid OuterComposite body);

    @POST
    @Path("/fake/outer/number")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @Operation(summary = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output number", content = @Content(schema = @Schema(implementation = BigDecimal.class))) })
    public BigDecimal fakeOuterNumberSerialize(@Valid BigDecimal body);

    @POST
    @Path("/fake/outer/string")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @Operation(summary = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output string", content = @Content(schema = @Schema(implementation = String.class))) })
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
    @Operation(summary = "To test \"client\" model", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Client.class))) })
    public Client testClientModel(@Valid Client body);

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     */
    @POST
    @Path("/fake")
    @Consumes({ "application/xml; charset=utf-8", "application/json; charset=utf-8" })
    @Operation(summary = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        @ApiResponse(responseCode = "404", description = "User not found") })
    public void testEndpointParameters(@Valid Body2 body);

    /**
     * To test enum parameters
     *
     * To test enum parameters
     *
     */
    @GET
    @Path("/fake")
    @Operation(summary = "To test enum parameters", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid request"),
        @ApiResponse(responseCode = "404", description = "Not found") })
    public void testEnumParameters(@HeaderParam("enum_header_string_array") List<String> enumHeaderStringArray, @HeaderParam("enum_header_string") String enumHeaderString, @QueryParam("enum_query_string_array") List<String> enumQueryStringArray, @QueryParam("enum_query_string") @DefaultValue("-efg") String enumQueryString, @QueryParam("enum_query_integer") Integer enumQueryInteger);

    /**
     * To test enum parameters
     *
     * To test enum parameters
     *
     */
    @POST
    @Path("/fake/enum/form")
    @Consumes({ "*/*" })
    @Operation(summary = "To test enum parameters", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid request"),
        @ApiResponse(responseCode = "404", description = "Not found") })
    public void testEnumRequestBody(@Valid Body4 body);

    /**
     * test inline additionalProperties
     *
     */
    @POST
    @Path("/fake/inline-additionalProperties")
    @Consumes({ "application/json" })
    @Operation(summary = "test inline additionalProperties", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public void testInlineAdditionalProperties(@Valid Map<String, String> body);

    /**
     * test json serialization of form data
     *
     */
    @POST
    @Path("/fake/jsonFormData")
    @Consumes({ "application/json" })
    @Operation(summary = "test json serialization of form data", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public void testJsonFormData(@Valid Body5 body);
}
