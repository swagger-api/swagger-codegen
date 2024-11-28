package io.swagger.client.api;

import io.swagger.client.ApiClient;
import io.swagger.client.model.Order;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for StoreApi
 */
public class StoreApiTest {

    private StoreApi api;

    @Before
    public void setup() {
        api = new ApiClient().createService(StoreApi.class);
    }


    /**
     * Delete purchase order by ID
     *
     * For valid response try integer IDs with positive integer value.\\ \\ Negative or non-integer values will generate API errors
     */
    @Test
    public void deleteOrderTest() {
        Long orderId = null;
        // Void response = api.deleteOrder(orderId);

        // TODO: test validations
    }

    /**
     * Returns pet inventories by status
     *
     * Returns a map of status codes to quantities
     */
    @Test
    public void getInventoryTest() {
        // Map<String, Integer> response = api.getInventory();

        // TODO: test validations
    }

    /**
     * Find purchase order by ID
     *
     * For valid response try integer IDs with value &gt;&#x3D; 1 and &lt;&#x3D; 10.\\ \\ Other values will generated exceptions
     */
    @Test
    public void getOrderByIdTest() {
        Long orderId = null;
        // Order response = api.getOrderById(orderId);

        // TODO: test validations
    }

    /**
     * Place an order for a pet
     *
     * 
     */
    @Test
    public void placeOrderTest() {
        Order body = null;
        // Order response = api.placeOrder(body);

        // TODO: test validations
    }
}
