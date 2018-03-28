package io.swagger.client.api;

import java.math.BigDecimal;
import io.swagger.client.model.Client;
import io.swagger.client.model.OuterComposite;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface FakeApi {

    
    
    void fakeOuterBooleanSerialize(Boolean body, Handler<AsyncResult<Boolean>> handler);

    
    void fakeOuterCompositeSerialize(OuterComposite outercomposite, Handler<AsyncResult<OuterComposite>> handler);

    
    void fakeOuterNumberSerialize(BigDecimal body, Handler<AsyncResult<BigDecimal>> handler);

    
    void fakeOuterStringSerialize(String body, Handler<AsyncResult<String>> handler);

    
    void testClientModel(Client client, Handler<AsyncResult<Client>> handler);

    
    void testEndpointParameters(Object body, Handler<AsyncResult<Void>> handler);

    
    void testEnumParameters(Object body, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Handler<AsyncResult<Void>> handler);

    
    void testInlineAdditionalProperties(Map<String, String> body, Handler<AsyncResult<Void>> handler);

    
    void testJsonFormData(Object body, Handler<AsyncResult<Void>> handler);

    
    
}
