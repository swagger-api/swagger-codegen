package io.swagger.client.auth;

import java.util.HashMap;
import java.util.Map;

import org.junit.*;
import static org.junit.Assert.*;


public class ApiKeyAuthTest {
    Map<String, Object> queryParams;
    Map<String, Object> headerParams;

    @Before
    public void setup() {
        queryParams = new HashMap<String, Object>();
        headerParams = new HashMap<String, Object>();
    }

    @Test
    public void testApplyToParamsInQuery() {
        ApiKeyAuth auth = new ApiKeyAuth("query", "api_key");
        auth.setApiKey("my-api-key");
        auth.applyToParams(queryParams, headerParams);

        assertEquals(1, queryParams.size());
        assertEquals("my-api-key", queryParams.get("api_key"));
        // no changes to header parameters
        assertEquals(0, headerParams.size());
    }

    @Test
    public void testApplyToParamsInHeaderWithPrefix() {
        ApiKeyAuth auth = new ApiKeyAuth("header", "X-API-TOKEN");
        auth.setApiKey("my-api-token");
        auth.setApiKeyPrefix("Token");
        auth.applyToParams(queryParams, headerParams);

        // no changes to query parameters
        assertEquals(0, queryParams.size());
        assertEquals(1, headerParams.size());
        assertEquals("Token my-api-token", headerParams.get("X-API-TOKEN"));
    }
}
