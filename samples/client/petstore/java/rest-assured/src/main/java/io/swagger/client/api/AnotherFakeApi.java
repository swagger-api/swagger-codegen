package io.swagger.client.api;

import io.swagger.client.model.Client;

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

public class AnotherFakeApi {

    private RequestSpecBuilder reqSpec;

    private AnotherFakeApi(RequestSpecBuilder reqSpec) {
        this.reqSpec = reqSpec;
    }

    public static AnotherFakeApi anotherFake(RequestSpecBuilder reqSpec) {
        return new AnotherFakeApi(reqSpec);
    }

    public TestSpecialTagsOper testSpecialTags() {
        return new TestSpecialTagsOper(reqSpec);
    }


    /**
     * To test special tags
     * To test special tags
     *
     * @see #body client model (required)
     * return Client
     */
    public class TestSpecialTagsOper {

        public static final String REQ_URI = "/another-fake/dummy";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public TestSpecialTagsOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public TestSpecialTagsOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * PATCH /another-fake/dummy
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(PATCH, REQ_URI));
        }

        /**
         * PATCH /another-fake/dummy
         * @return Client
         */
        public Client executeAs(Function<Response, Response> handler) {
            return execute(handler).as(Client.class, GSON);
        }


         /**
         * @param body client model (required)
         */
        public TestSpecialTagsOper body(Client body) {
            reqSpec.setBody(body, GSON);
            return this;
        }





        /**
         * Customise request specification
         */
        public TestSpecialTagsOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public TestSpecialTagsOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
}