package io.swagger.client.api;

import java.math.BigDecimal;
import io.swagger.client.model.Client;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;
import io.swagger.client.model.OuterComposite;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import io.restassured.RestAssured;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.builder.ResponseSpecBuilder;
import io.restassured.response.Response;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import static io.restassured.http.Method.*;
import static io.restassured.mapper.ObjectMapperType.GSON;

public class FakeApi {

    private RequestSpecBuilder reqSpec;

    private FakeApi(RequestSpecBuilder reqSpec) {
        this.reqSpec = reqSpec;
    }

    public static FakeApi fake(RequestSpecBuilder reqSpec) {
        return new FakeApi(reqSpec);
    }

    public FakeOuterBooleanSerializeOper fakeOuterBooleanSerialize() {
        return new FakeOuterBooleanSerializeOper(reqSpec);
    }

    public FakeOuterCompositeSerializeOper fakeOuterCompositeSerialize() {
        return new FakeOuterCompositeSerializeOper(reqSpec);
    }

    public FakeOuterNumberSerializeOper fakeOuterNumberSerialize() {
        return new FakeOuterNumberSerializeOper(reqSpec);
    }

    public FakeOuterStringSerializeOper fakeOuterStringSerialize() {
        return new FakeOuterStringSerializeOper(reqSpec);
    }

    public TestClientModelOper testClientModel() {
        return new TestClientModelOper(reqSpec);
    }

    public TestEndpointParametersOper testEndpointParameters() {
        return new TestEndpointParametersOper(reqSpec);
    }

    public TestEnumParametersOper testEnumParameters() {
        return new TestEnumParametersOper(reqSpec);
    }

    public TestInlineAdditionalPropertiesOper testInlineAdditionalProperties() {
        return new TestInlineAdditionalPropertiesOper(reqSpec);
    }

    public TestJsonFormDataOper testJsonFormData() {
        return new TestJsonFormDataOper(reqSpec);
    }


    /**
     * 
     * Test serialization of outer boolean types
     *
     * @see #body Input boolean as post body (optional)
     * return Boolean
     */
    public class FakeOuterBooleanSerializeOper {

        public static final String REQ_URI = "/fake/outer/boolean";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public FakeOuterBooleanSerializeOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public FakeOuterBooleanSerializeOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * POST /fake/outer/boolean
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(POST, REQ_URI));
        }

        /**
         * POST /fake/outer/boolean
         * @return Boolean
         */
        public Boolean executeAs(Function<Response, Response> handler) {
            return execute(handler).as(Boolean.class, GSON);
        }


         /**
         * @param body Input boolean as post body (optional)
         */
        public FakeOuterBooleanSerializeOper body(Boolean body) {
            reqSpec.setBody(body, GSON);
            return this;
        }





        /**
         * Customise request specification
         */
        public FakeOuterBooleanSerializeOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public FakeOuterBooleanSerializeOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * 
     * Test serialization of object with outer number type
     *
     * @see #body Input composite as post body (optional)
     * return OuterComposite
     */
    public class FakeOuterCompositeSerializeOper {

        public static final String REQ_URI = "/fake/outer/composite";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public FakeOuterCompositeSerializeOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public FakeOuterCompositeSerializeOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * POST /fake/outer/composite
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(POST, REQ_URI));
        }

        /**
         * POST /fake/outer/composite
         * @return OuterComposite
         */
        public OuterComposite executeAs(Function<Response, Response> handler) {
            return execute(handler).as(OuterComposite.class, GSON);
        }


         /**
         * @param body Input composite as post body (optional)
         */
        public FakeOuterCompositeSerializeOper body(OuterComposite body) {
            reqSpec.setBody(body, GSON);
            return this;
        }





        /**
         * Customise request specification
         */
        public FakeOuterCompositeSerializeOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public FakeOuterCompositeSerializeOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * 
     * Test serialization of outer number types
     *
     * @see #body Input number as post body (optional)
     * return BigDecimal
     */
    public class FakeOuterNumberSerializeOper {

        public static final String REQ_URI = "/fake/outer/number";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public FakeOuterNumberSerializeOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public FakeOuterNumberSerializeOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * POST /fake/outer/number
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(POST, REQ_URI));
        }

        /**
         * POST /fake/outer/number
         * @return BigDecimal
         */
        public BigDecimal executeAs(Function<Response, Response> handler) {
            return execute(handler).as(BigDecimal.class, GSON);
        }


         /**
         * @param body Input number as post body (optional)
         */
        public FakeOuterNumberSerializeOper body(BigDecimal body) {
            reqSpec.setBody(body, GSON);
            return this;
        }





        /**
         * Customise request specification
         */
        public FakeOuterNumberSerializeOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public FakeOuterNumberSerializeOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * 
     * Test serialization of outer string types
     *
     * @see #body Input string as post body (optional)
     * return String
     */
    public class FakeOuterStringSerializeOper {

        public static final String REQ_URI = "/fake/outer/string";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public FakeOuterStringSerializeOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public FakeOuterStringSerializeOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * POST /fake/outer/string
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(POST, REQ_URI));
        }

        /**
         * POST /fake/outer/string
         * @return String
         */
        public String executeAs(Function<Response, Response> handler) {
            return execute(handler).as(String.class, GSON);
        }


         /**
         * @param body Input string as post body (optional)
         */
        public FakeOuterStringSerializeOper body(String body) {
            reqSpec.setBody(body, GSON);
            return this;
        }





        /**
         * Customise request specification
         */
        public FakeOuterStringSerializeOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public FakeOuterStringSerializeOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     *
     * @see #body client model (required)
     * return Client
     */
    public class TestClientModelOper {

        public static final String REQ_URI = "/fake";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public TestClientModelOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public TestClientModelOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * PATCH /fake
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(PATCH, REQ_URI));
        }

        /**
         * PATCH /fake
         * @return Client
         */
        public Client executeAs(Function<Response, Response> handler) {
            return execute(handler).as(Client.class, GSON);
        }


         /**
         * @param body client model (required)
         */
        public TestClientModelOper body(Client body) {
            reqSpec.setBody(body, GSON);
            return this;
        }





        /**
         * Customise request specification
         */
        public TestClientModelOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public TestClientModelOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     * @see #numberForm None (required)
     * @see #_doubleForm None (required)
     * @see #patternWithoutDelimiterForm None (required)
     * @see #_byteForm None (required)
     * @see #integerForm None (optional)
     * @see #int32Form None (optional)
     * @see #int64Form None (optional)
     * @see #_floatForm None (optional)
     * @see #stringForm None (optional)
     * @see #binaryForm None (optional)
     * @see #dateForm None (optional)
     * @see #dateTimeForm None (optional)
     * @see #passwordForm None (optional)
     * @see #paramCallbackForm None (optional)
     */
    public class TestEndpointParametersOper {

        public static final String REQ_URI = "/fake";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public TestEndpointParametersOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public TestEndpointParametersOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * POST /fake
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(POST, REQ_URI));
        }






         /**
         * @param integer None (optional)
         */
         public TestEndpointParametersOper integerForm(Integer integer) {
            reqSpec.addFormParam("integer", integer);
            return this;
         }
         /**
         * @param int32 None (optional)
         */
         public TestEndpointParametersOper int32Form(Integer int32) {
            reqSpec.addFormParam("int32", int32);
            return this;
         }
         /**
         * @param int64 None (optional)
         */
         public TestEndpointParametersOper int64Form(Long int64) {
            reqSpec.addFormParam("int64", int64);
            return this;
         }
         /**
         * @param number None (required)
         */
         public TestEndpointParametersOper numberForm(BigDecimal number) {
            reqSpec.addFormParam("number", number);
            return this;
         }
         /**
         * @param _float None (optional)
         */
         public TestEndpointParametersOper _floatForm(Float _float) {
            reqSpec.addFormParam("float", _float);
            return this;
         }
         /**
         * @param _double None (required)
         */
         public TestEndpointParametersOper _doubleForm(Double _double) {
            reqSpec.addFormParam("double", _double);
            return this;
         }
         /**
         * @param string None (optional)
         */
         public TestEndpointParametersOper stringForm(String string) {
            reqSpec.addFormParam("string", string);
            return this;
         }
         /**
         * @param patternWithoutDelimiter None (required)
         */
         public TestEndpointParametersOper patternWithoutDelimiterForm(String patternWithoutDelimiter) {
            reqSpec.addFormParam("pattern_without_delimiter", patternWithoutDelimiter);
            return this;
         }
         /**
         * @param _byte None (required)
         */
         public TestEndpointParametersOper _byteForm(byte[] _byte) {
            reqSpec.addFormParam("byte", _byte);
            return this;
         }
         /**
         * @param binary None (optional)
         */
         public TestEndpointParametersOper binaryForm(byte[] binary) {
            reqSpec.addFormParam("binary", binary);
            return this;
         }
         /**
         * @param date None (optional)
         */
         public TestEndpointParametersOper dateForm(LocalDate date) {
            reqSpec.addFormParam("date", date);
            return this;
         }
         /**
         * @param dateTime None (optional)
         */
         public TestEndpointParametersOper dateTimeForm(OffsetDateTime dateTime) {
            reqSpec.addFormParam("dateTime", dateTime);
            return this;
         }
         /**
         * @param password None (optional)
         */
         public TestEndpointParametersOper passwordForm(String password) {
            reqSpec.addFormParam("password", password);
            return this;
         }
         /**
         * @param paramCallback None (optional)
         */
         public TestEndpointParametersOper paramCallbackForm(String paramCallback) {
            reqSpec.addFormParam("callback", paramCallback);
            return this;
         }

        /**
         * Customise request specification
         */
        public TestEndpointParametersOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public TestEndpointParametersOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * To test enum parameters
     * To test enum parameters
     *
     * @see #enumFormStringArrayForm Form parameter enum test (string array) (optional)
     * @see #enumFormStringForm Form parameter enum test (string) (optional, default to -efg)
     * @see #enumHeaderStringArrayHeader Header parameter enum test (string array) (optional)
     * @see #enumHeaderStringHeader Header parameter enum test (string) (optional, default to -efg)
     * @see #enumQueryStringArrayQuery Query parameter enum test (string array) (optional)
     * @see #enumQueryStringQuery Query parameter enum test (string) (optional, default to -efg)
     * @see #enumQueryIntegerQuery Query parameter enum test (double) (optional)
     * @see #enumQueryDoubleForm Query parameter enum test (double) (optional)
     */
    public class TestEnumParametersOper {

        public static final String REQ_URI = "/fake";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public TestEnumParametersOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public TestEnumParametersOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * GET /fake
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(GET, REQ_URI));
        }



        /**
         * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
         */
        public TestEnumParametersOper enumHeaderStringArrayHeader(String enumHeaderStringArray) {
            reqSpec.addHeader("enum_header_string_array", enumHeaderStringArray);
            return this;
        }
        /**
         * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
         */
        public TestEnumParametersOper enumHeaderStringHeader(String enumHeaderString) {
            reqSpec.addHeader("enum_header_string", enumHeaderString);
            return this;
        }


        /**
         * @param enumQueryStringArray Query parameter enum test (string array) (optional)
         */
        public TestEnumParametersOper enumQueryStringArrayQuery(List<String> enumQueryStringArray) {
            reqSpec.addQueryParam("enum_query_string_array", enumQueryStringArray);
            return this;
        }
        /**
         * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
         */
        public TestEnumParametersOper enumQueryStringQuery(String enumQueryString) {
            reqSpec.addQueryParam("enum_query_string", enumQueryString);
            return this;
        }
        /**
         * @param enumQueryInteger Query parameter enum test (double) (optional)
         */
        public TestEnumParametersOper enumQueryIntegerQuery(Integer enumQueryInteger) {
            reqSpec.addQueryParam("enum_query_integer", enumQueryInteger);
            return this;
        }

         /**
         * @param enumFormStringArray Form parameter enum test (string array) (optional)
         */
         public TestEnumParametersOper enumFormStringArrayForm(List<String> enumFormStringArray) {
            reqSpec.addFormParam("enum_form_string_array", enumFormStringArray);
            return this;
         }
         /**
         * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
         */
         public TestEnumParametersOper enumFormStringForm(String enumFormString) {
            reqSpec.addFormParam("enum_form_string", enumFormString);
            return this;
         }
         /**
         * @param enumQueryDouble Query parameter enum test (double) (optional)
         */
         public TestEnumParametersOper enumQueryDoubleForm(Double enumQueryDouble) {
            reqSpec.addFormParam("enum_query_double", enumQueryDouble);
            return this;
         }

        /**
         * Customise request specification
         */
        public TestEnumParametersOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public TestEnumParametersOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * test inline additionalProperties
     * 
     *
     * @see #body request body (required)
     */
    public class TestInlineAdditionalPropertiesOper {

        public static final String REQ_URI = "/fake/inline-additionalProperties";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public TestInlineAdditionalPropertiesOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public TestInlineAdditionalPropertiesOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * POST /fake/inline-additionalProperties
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(POST, REQ_URI));
        }


         /**
         * @param param request body (required)
         */
        public TestInlineAdditionalPropertiesOper body(Object param) {
            reqSpec.setBody(param, GSON);
            return this;
        }





        /**
         * Customise request specification
         */
        public TestInlineAdditionalPropertiesOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public TestInlineAdditionalPropertiesOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * test json serialization of form data
     * 
     *
     * @see #paramForm field1 (required)
     * @see #param2Form field2 (required)
     */
    public class TestJsonFormDataOper {

        public static final String REQ_URI = "/fake/jsonFormData";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public TestJsonFormDataOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public TestJsonFormDataOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * GET /fake/jsonFormData
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(GET, REQ_URI));
        }






         /**
         * @param param field1 (required)
         */
         public TestJsonFormDataOper paramForm(String param) {
            reqSpec.addFormParam("param", param);
            return this;
         }
         /**
         * @param param2 field2 (required)
         */
         public TestJsonFormDataOper param2Form(String param2) {
            reqSpec.addFormParam("param2", param2);
            return this;
         }

        /**
         * Customise request specification
         */
        public TestJsonFormDataOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public TestJsonFormDataOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
}