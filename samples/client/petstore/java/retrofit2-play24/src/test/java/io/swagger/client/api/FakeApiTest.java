package io.swagger.client.api;

import io.swagger.client.ApiClient;
import java.math.BigDecimal;
import io.swagger.client.model.Client;
import io.swagger.client.model.OuterComposite;

import org.junit.Before;
import org.junit.Test;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * API tests for FakeApi
 */
public class FakeApiTest {

    private FakeApi api;

    @Before
    public void setup() {
        api = new ApiClient().createService(FakeApi.class);
    }

    
    
    /**
     * 
     *
     * Test serialization of outer boolean types
     */
    @Test
    public void fakeOuterBooleanSerializeTest() {
        
        Boolean body = null;
        
        // Boolean response = api.fakeOuterBooleanSerialize(body);

        // TODO: test validations
    }
    
    /**
     * 
     *
     * Test serialization of object with outer number type
     */
    @Test
    public void fakeOuterCompositeSerializeTest() {
        
        OuterComposite outercomposite = null;
        
        // OuterComposite response = api.fakeOuterCompositeSerialize(outercomposite);

        // TODO: test validations
    }
    
    /**
     * 
     *
     * Test serialization of outer number types
     */
    @Test
    public void fakeOuterNumberSerializeTest() {
        
        BigDecimal body = null;
        
        // BigDecimal response = api.fakeOuterNumberSerialize(body);

        // TODO: test validations
    }
    
    /**
     * 
     *
     * Test serialization of outer string types
     */
    @Test
    public void fakeOuterStringSerializeTest() {
        
        String body = null;
        
        // String response = api.fakeOuterStringSerialize(body);

        // TODO: test validations
    }
    
    /**
     * To test \&quot;client\&quot; model
     *
     * To test \&quot;client\&quot; model
     */
    @Test
    public void testClientModelTest() {
        
        Client client = null;
        
        // Client response = api.testClientModel(client);

        // TODO: test validations
    }
    
    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     */
    @Test
    public void testEndpointParametersTest() {
        
        Object body = null;
        
        // Void response = api.testEndpointParameters(body);

        // TODO: test validations
    }
    
    /**
     * To test enum parameters
     *
     * To test enum parameters
     */
    @Test
    public void testEnumParametersTest() {
        
        Object body = null;
        
        List<String> enumHeaderStringArray = null;
        
        String enumHeaderString = null;
        
        List<String> enumQueryStringArray = null;
        
        String enumQueryString = null;
        
        Integer enumQueryInteger = null;
        
        // Void response = api.testEnumParameters(body, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger);

        // TODO: test validations
    }
    
    /**
     * test inline additionalProperties
     *
     * 
     */
    @Test
    public void testInlineAdditionalPropertiesTest() {
        
        Map<String, String> body = null;
        
        // Void response = api.testInlineAdditionalProperties(body);

        // TODO: test validations
    }
    
    /**
     * test json serialization of form data
     *
     * 
     */
    @Test
    public void testJsonFormDataTest() {
        
        Object body = null;
        
        // Void response = api.testJsonFormData(body);

        // TODO: test validations
    }
    
    
}
