package io.swagger.client.api;

import io.swagger.client.ApiClient;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for DefaultApi
 */
public class DefaultApiTest {

    private DefaultApi api;

    @Before
    public void setup() {
        api = new ApiClient().createService(DefaultApi.class);
    }


    /**
     * 
     *
     * 
     */
    @Test
    public void testMethodTest() {
        // List<String> response = api.testMethod();

        // TODO: test validations
    }
}
