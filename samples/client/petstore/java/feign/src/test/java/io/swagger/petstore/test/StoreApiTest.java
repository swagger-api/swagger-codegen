package io.swagger.petstore.test;

import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;
import java.util.Map;

import feign.FeignException;
import io.swagger.TestUtils;
import io.swagger.client.ApiClient;
import io.swagger.client.api.StoreApi;
import io.swagger.client.model.Order;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class StoreApiTest {
    ApiClient apiClient;
    StoreApi api;

    @Before
    public void setup() {
        apiClient = new ApiClient();
        api = apiClient.buildClient(StoreApi.class);
    }

    @Test
    public void testGetInventory() throws Exception {
        Map<String, Integer> inventory = api.getInventory();
        assertTrue(inventory.keySet().size() > 0);
    }

    @Test
    public void testPlaceOrder() throws Exception {
        Order order = createOrder();
        api.placeOrder(order);

        Order fetched = api.getOrderById(order.getId());
        assertEquals(order.getId(), fetched.getId());
        assertEquals(order.getPetId(), fetched.getPetId());
        assertEquals(order.getQuantity(), fetched.getQuantity());
    }

    @Test
    public void testDeleteOrder() throws Exception {
        Order order = createOrder();
        api.placeOrder(order);

        Order fetched = api.getOrderById(order.getId());
        assertEquals(fetched.getId(), order.getId());

        api.deleteOrder(order.getId().toString());

        try {
            api.getOrderById(order.getId());
            fail("expected an error");
        } catch (FeignException e) {
            assertTrue(e.getMessage().startsWith("status 404 "));
        }
    }

    private Order createOrder() {
        Order order = new Order();
        order.setPetId(new Long(200));
        order.setQuantity(new Integer(13));
        order.setShipDate(new java.util.Date());
        order.setStatus(Order.StatusEnum.PLACED);
        order.setComplete(true);

        try {
            Field idField = Order.class.getDeclaredField("id");
            idField.setAccessible(true);
            idField.set(order, TestUtils.nextId());
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        return order;
    }
}
