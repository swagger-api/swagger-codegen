package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Client;
import org.joda.time.LocalDate;

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
import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.PATCH;
import javax.validation.constraints.*;

@Path("/")
@Api(value = "/", description = "")
public interface FakeApi  {

    @PATCH
    @Path("/fake")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test \"client\" model", tags={ "fake",  })
    public Client testClientModel(@ApiParam(value = "client model", required=true) Client body);

    @POST
    @Path("/fake")
    @Consumes({ "application/xml; charset=utf-8", "application/json; charset=utf-8" })
    @Produces({ "application/xml; charset=utf-8", "application/json; charset=utf-8" })
    @ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", tags={ "fake",  })
    public void testEndpointParameters(@ApiParam(value = "None", required=true) @Multipart(value = "number")  BigDecimal number, @ApiParam(value = "None", required=true) @Multipart(value = "_double")  Double _double, @ApiParam(value = "None", required=true) @Multipart(value = "patternWithoutDelimiter")  String patternWithoutDelimiter, @ApiParam(value = "None", required=true) @Multipart(value = "_byte")  byte[] _byte, @ApiParam(value = "None") @Multipart(value = "integer", required = false)  Integer integer, @ApiParam(value = "None") @Multipart(value = "int32", required = false)  Integer int32, @ApiParam(value = "None") @Multipart(value = "int64", required = false)  Long int64, @ApiParam(value = "None") @Multipart(value = "_float", required = false)  Float _float, @ApiParam(value = "None") @Multipart(value = "string", required = false)  String string, @ApiParam(value = "None") @Multipart(value = "binary", required = false)  byte[] binary, @ApiParam(value = "None") @Multipart(value = "date", required = false)  LocalDate date, @ApiParam(value = "None") @Multipart(value = "dateTime", required = false)  javax.xml.datatype.XMLGregorianCalendar dateTime, @ApiParam(value = "None") @Multipart(value = "password", required = false)  String password, @ApiParam(value = "None") @Multipart(value = "paramCallback", required = false)  String paramCallback);

    @GET
    @Path("/fake")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @ApiOperation(value = "To test enum parameters", tags={ "fake" })
    public void testEnumParameters(@ApiParam(value = "Form parameter enum test (string array)", allowableValues=">, $") @Multipart(value = "enumFormStringArray", required = false)  List<String> enumFormStringArray, @ApiParam(value = "Form parameter enum test (string)", allowableValues="_abc, -efg, (xyz)", defaultValue="-efg") @Multipart(value = "enumFormString", required = false)  String enumFormString, @HeaderParam("enum_header_string_array") List<String> enumHeaderStringArray, @HeaderParam("enum_header_string") String enumHeaderString, @ApiParam(value = "Query parameter enum test (string array)", allowableValues=">, $") @QueryParam("enum_query_string_array")  List<String> enumQueryStringArray, @ApiParam(value = "Query parameter enum test (string)", allowableValues="_abc, -efg, (xyz)", defaultValue="-efg") @QueryParam("enum_query_string")  String enumQueryString, @ApiParam(value = "Query parameter enum test (double)") @QueryParam("enum_query_integer")  Integer enumQueryInteger, @ApiParam(value = "Query parameter enum test (double)") @Multipart(value = "enumQueryDouble", required = false)  Double enumQueryDouble);
}

