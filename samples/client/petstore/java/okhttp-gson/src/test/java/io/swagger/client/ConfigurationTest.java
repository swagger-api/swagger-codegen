package io.swagger.client;

import org.junit.*;
import static org.junit.Assert.*;


public class ConfigurationTest {
    @Test
    public void testDefaultApiClient() {
        ApiClient apiClient = Configuration.getDefaultApiClient();
        assertNotNull(apiClient);
        assertEquals("petstore.swagger.io:80", apiClient.getHost());
        assertEquals("/v2", apiClient.getBasePath());
        assertFalse(apiClient.isDebugging());
    }
}
