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

public class FakeClassnameTags123Api {

    private RequestSpecBuilder reqSpec;

    private FakeClassnameTags123Api(RequestSpecBuilder reqSpec) {
        this.reqSpec = reqSpec;
    }

    public static FakeClassnameTags123Api fakeClassnameTags123(RequestSpecBuilder reqSpec) {
        return new FakeClassnameTags123Api(reqSpec);
    }

    public TestClassnameOper testClassname() {
        return new TestClassnameOper(reqSpec);
    }


    /**
     * To test class name in snake case
     * 
     *
     * @see #body client model (required)
     * return Client
     */
    public class TestClassnameOper {

        public static final String REQ_URI = "/fake_classname_test";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public TestClassnameOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public TestClassnameOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * PATCH /fake_classname_test
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(PATCH, REQ_URI));
        }

        /**
         * PATCH /fake_classname_test
         * @return Client
         */
        public Client executeAs(Function<Response, Response> handler) {
            return execute(handler).as(Client.class, GSON);
        }


         /**
         * @param body client model (required)
         */
        public TestClassnameOper body(Client body) {
            reqSpec.setBody(body, GSON);
            return this;
        }





        /**
         * Customise request specification
         */
        public TestClassnameOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public TestClassnameOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
}