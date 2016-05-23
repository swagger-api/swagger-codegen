package io.swagger.client;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;


public class ConfigurationTest {
    @Test
    public void testDefaultApiClient() {
        ApiClient apiClient = Configuration.getDefaultApiClient();
        assertNotNull(apiClient);
        assertEquals("http://petstore.swagger.io/v2", apiClient.getBasePath());
        assertFalse(apiClient.isDebugging());
    }
}
