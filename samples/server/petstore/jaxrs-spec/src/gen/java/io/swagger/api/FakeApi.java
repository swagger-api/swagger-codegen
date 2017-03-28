package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Client;
import java.util.Date;
import org.joda.time.LocalDate;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;
import javax.validation.constraints.*;

@Path("/fake")

@Api(description = "the fake API")




public class FakeApi  {
    


    @PATCH
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test \"client\" model", notes = "To test \"client\" model", response = Client.class, tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    public Response testClientModel(Client body) {
        return Response.ok().entity("magic!").build();
    }
    


    @POST
    
    @Consumes({ "application/xml; charset&#x3D;utf-8", "application/json; charset&#x3D;utf-8" })
    @Produces({ "application/xml; charset&#x3D;utf-8", "application/json; charset&#x3D;utf-8" })
    @ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", notes = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", response = void.class, authorizations = {
        @Authorization(value = "http_basic_test")
    }, tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = void.class),
        @ApiResponse(code = 404, message = "User not found", response = void.class) })
    public Response testEndpointParameters(@FormParam(value = "number")  BigDecimal number,@FormParam(value = "double")  Double _double,@FormParam(value = "pattern_without_delimiter")  String patternWithoutDelimiter,@FormParam(value = "byte")  byte[] _byte,@FormParam(value = "integer")  Integer integer,@FormParam(value = "int32")  Integer int32,@FormParam(value = "int64")  Long int64,@FormParam(value = "float")  Float _float,@FormParam(value = "string")  String string,@FormParam(value = "binary")  byte[] binary,@FormParam(value = "date")  LocalDate date,@FormParam(value = "dateTime")  Date dateTime,@FormParam(value = "password")  String password,@FormParam(value = "callback")  String paramCallback) {
        return Response.ok().entity("magic!").build();
    }
    
    public enum EnumQueryStringArrayEnum {

        GREATER_THAN(String.valueOf(">")), DOLLAR(String.valueOf("$"));


        private String value;

        EnumQueryStringArrayEnum (String v) {
            value = v;
        }

        public String value() {
            return toString();
        }

        @Override
        public String toString() {
            return String.valueOf(value);
        }

        public static EnumQueryStringArrayEnum fromValue(String v) {
            for (EnumQueryStringArrayEnum b : EnumQueryStringArrayEnum.values()) {
                if (String.valueOf(b.value).equals(v)) {
                    return b;
                }
            }
            return null;
        }

        public static EnumQueryStringArrayEnum fromString(String v) {
            return fromValue(v);
        }
    }

    public enum EnumQueryStringEnum {

        _ABC(String.valueOf("_abc")), _EFG(String.valueOf("-efg")), _XYZ_(String.valueOf("(xyz)"));


        private String value;

        EnumQueryStringEnum (String v) {
            value = v;
        }

        public String value() {
            return toString();
        }

        @Override
        public String toString() {
            return String.valueOf(value);
        }

        public static EnumQueryStringEnum fromValue(String v) {
            for (EnumQueryStringEnum b : EnumQueryStringEnum.values()) {
                if (String.valueOf(b.value).equals(v)) {
                    return b;
                }
            }
            return null;
        }

        public static EnumQueryStringEnum fromString(String v) {
            return fromValue(v);
        }
    }

    public enum EnumQueryIntegerEnum {

        NUMBER_1(Integer.valueOf(1)), NUMBER_MINUS_2(Integer.valueOf(-2));


        private Integer value;

        EnumQueryIntegerEnum (Integer v) {
            value = v;
        }

        public String value() {
            return toString();
        }

        @Override
        public String toString() {
            return String.valueOf(value);
        }

        public static EnumQueryIntegerEnum fromValue(String v) {
            for (EnumQueryIntegerEnum b : EnumQueryIntegerEnum.values()) {
                if (String.valueOf(b.value).equals(v)) {
                    return b;
                }
            }
            return null;
        }

        public static EnumQueryIntegerEnum fromString(String v) {
            return fromValue(v);
        }
    }



    @GET
    
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @ApiOperation(value = "To test enum parameters", notes = "To test enum parameters", response = void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid request", response = void.class),
        @ApiResponse(code = 404, message = "Not found", response = void.class) })
    public Response testEnumParameters(@FormParam(value = "enum_form_string_array")  List<String> enumFormStringArray,@FormParam(value = "enum_form_string")  String enumFormString,@HeaderParam("enum_header_string_array") List<String> enumHeaderStringArray,@HeaderParam("enum_header_string") String enumHeaderString,@ApiParam(value = "Query parameter enum test (string array)", allowableValues=">, $") @QueryParam("enum_query_string_array")  List<EnumQueryStringArrayEnum> enumQueryStringArray,@ApiParam(value = "Query parameter enum test (string)", allowableValues="_abc, -efg, (xyz)", defaultValue="-efg") @QueryParam("enum_query_string")  @DefaultValue("-efg") EnumQueryStringEnum enumQueryString,@ApiParam(value = "Query parameter enum test (double)", allowableValues="1, -2") @QueryParam("enum_query_integer")  EnumQueryIntegerEnum enumQueryInteger,@FormParam(value = "enum_query_double")  Double enumQueryDouble) {
        return Response.ok().entity("magic!").build();
    }
}

