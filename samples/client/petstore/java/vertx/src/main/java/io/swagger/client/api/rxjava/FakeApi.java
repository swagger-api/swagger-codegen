package io.swagger.client.api.rxjava;

import java.math.BigDecimal;
import io.swagger.client.model.Client;
import io.swagger.client.model.OuterComposite;


import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;



public class FakeApi {

	private final io.swagger.client.api.FakeApi delegate;

	public FakeApi(io.swagger.client.api.FakeApi delegate) {
	    this.delegate = delegate;
    }

	public io.swagger.client.api.FakeApi getDelegate() {
	    return delegate;
	}

    
    /**
     * 
     * Test serialization of outer boolean types
     
     * @param body Input boolean as post body (optional)
     
     * @param resultHandler Asynchronous result handler
     */
    public void fakeOuterBooleanSerialize(Boolean body, Handler<AsyncResult<Boolean>> resultHandler) {
        delegate.fakeOuterBooleanSerialize(body, resultHandler);
    }

    /**
     * 
     * Test serialization of outer boolean types
     
     * @param body Input boolean as post body (optional)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Boolean> rxFakeOuterBooleanSerialize(Boolean body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.fakeOuterBooleanSerialize(body, fut);
        }));
    }
    
    /**
     * 
     * Test serialization of object with outer number type
     
     * @param outercomposite Input composite as post body (optional)
     
     * @param resultHandler Asynchronous result handler
     */
    public void fakeOuterCompositeSerialize(OuterComposite outercomposite, Handler<AsyncResult<OuterComposite>> resultHandler) {
        delegate.fakeOuterCompositeSerialize(outercomposite, resultHandler);
    }

    /**
     * 
     * Test serialization of object with outer number type
     
     * @param outercomposite Input composite as post body (optional)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<OuterComposite> rxFakeOuterCompositeSerialize(OuterComposite outercomposite) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.fakeOuterCompositeSerialize(outercomposite, fut);
        }));
    }
    
    /**
     * 
     * Test serialization of outer number types
     
     * @param body Input number as post body (optional)
     
     * @param resultHandler Asynchronous result handler
     */
    public void fakeOuterNumberSerialize(BigDecimal body, Handler<AsyncResult<BigDecimal>> resultHandler) {
        delegate.fakeOuterNumberSerialize(body, resultHandler);
    }

    /**
     * 
     * Test serialization of outer number types
     
     * @param body Input number as post body (optional)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<BigDecimal> rxFakeOuterNumberSerialize(BigDecimal body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.fakeOuterNumberSerialize(body, fut);
        }));
    }
    
    /**
     * 
     * Test serialization of outer string types
     
     * @param body Input string as post body (optional)
     
     * @param resultHandler Asynchronous result handler
     */
    public void fakeOuterStringSerialize(String body, Handler<AsyncResult<String>> resultHandler) {
        delegate.fakeOuterStringSerialize(body, resultHandler);
    }

    /**
     * 
     * Test serialization of outer string types
     
     * @param body Input string as post body (optional)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<String> rxFakeOuterStringSerialize(String body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.fakeOuterStringSerialize(body, fut);
        }));
    }
    
    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     
     * @param client client model (required)
     
     * @param resultHandler Asynchronous result handler
     */
    public void testClientModel(Client client, Handler<AsyncResult<Client>> resultHandler) {
        delegate.testClientModel(client, resultHandler);
    }

    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     
     * @param client client model (required)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Client> rxTestClientModel(Client client) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.testClientModel(client, fut);
        }));
    }
    
    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     
     * @param body  (required)
     
     * @param resultHandler Asynchronous result handler
     */
    public void testEndpointParameters(Object body, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testEndpointParameters(body, resultHandler);
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     
     * @param body  (required)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxTestEndpointParameters(Object body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.testEndpointParameters(body, fut);
        }));
    }
    
    /**
     * To test enum parameters
     * To test enum parameters
     
     * @param body  (optional)
     
     * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
     
     * @param enumHeaderString Header parameter enum test (string) (optional)
     
     * @param enumQueryStringArray Query parameter enum test (string array) (optional)
     
     * @param enumQueryString Query parameter enum test (string) (optional)
     
     * @param enumQueryInteger Query parameter enum test (double) (optional)
     
     * @param resultHandler Asynchronous result handler
     */
    public void testEnumParameters(Object body, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testEnumParameters(body, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, resultHandler);
    }

    /**
     * To test enum parameters
     * To test enum parameters
     
     * @param body  (optional)
     
     * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
     
     * @param enumHeaderString Header parameter enum test (string) (optional)
     
     * @param enumQueryStringArray Query parameter enum test (string array) (optional)
     
     * @param enumQueryString Query parameter enum test (string) (optional)
     
     * @param enumQueryInteger Query parameter enum test (double) (optional)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxTestEnumParameters(Object body, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.testEnumParameters(body, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, fut);
        }));
    }
    
    /**
     * test inline additionalProperties
     * 
     
     * @param body request body (required)
     
     * @param resultHandler Asynchronous result handler
     */
    public void testInlineAdditionalProperties(Map<String, String> body, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testInlineAdditionalProperties(body, resultHandler);
    }

    /**
     * test inline additionalProperties
     * 
     
     * @param body request body (required)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxTestInlineAdditionalProperties(Map<String, String> body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.testInlineAdditionalProperties(body, fut);
        }));
    }
    
    /**
     * test json serialization of form data
     * 
     
     * @param body  (required)
     
     * @param resultHandler Asynchronous result handler
     */
    public void testJsonFormData(Object body, Handler<AsyncResult<Void>> resultHandler) {
        delegate.testJsonFormData(body, resultHandler);
    }

    /**
     * test json serialization of form data
     * 
     
     * @param body  (required)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxTestJsonFormData(Object body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.testJsonFormData(body, fut);
        }));
    }
    

    public static FakeApi newInstance(io.swagger.client.api.FakeApi arg) {
        return arg != null ? new FakeApi(arg) : null;
    }
}

