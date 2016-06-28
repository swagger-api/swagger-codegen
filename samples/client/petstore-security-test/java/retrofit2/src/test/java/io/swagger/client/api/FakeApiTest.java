package io.swagger.client.api;

import io.swagger.client.ApiClient;
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
     * To test code injection  &#39; \&quot; &#x3D;end
     *
     * 
     */
    @Test
    public void testCodeInjectEndTest() {
        String testCodeInjectEnd = null;
        // Void response = api.testCodeInjectEnd(testCodeInjectEnd);

        // TODO: test validations
    }
    
}
