package io.swagger.client.api;

import io.swagger.client.ApiClient;
import io.swagger.client.model.Order;
import org.junit.Before;
import org.junit.Test;
import org.threeten.bp.OffsetDateTime;
import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

/**
 * API tests for StoreApi
 */
@SuppressWarnings("Duplicates")
public class StoreApiTest {

    private StoreApi api;

    @Before
    public void setup() {
        api = new ApiClient().createService(StoreApi.class);
    }

    /**
     * Delete purchase order by ID
     *
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     */
    @Test
    public void deleteOrderTest() throws IOException {
        Order order = createOrder();
        Order created = api.placeOrder(order)
                .execute()
                .body();
        assertNotNull(created);
        Order fetched = api.getOrderById(created.getId())
                .execute()
                .body();
        assertNotNull(fetched);
        assertEquals(fetched.getId(), order.getId());
        api.deleteOrder(String.valueOf(order.getId()))
                .execute();
        Order body = api.getOrderById(order.getId())
                .execute().body();
        assertNull(body);
    }

    /**
     * Returns pet inventories by status
     *
     * Returns a map of status codes to quantities
     */
    @Test
    public void getInventoryTest() throws IOException {
         Map<String, Integer> response = api.getInventory().execute().body();
         assertTrue(response.size() > 0);
    }

    /**
     * Place an order for a pet
     *
     * 
     */
    @Test
    public void placeOrderTest() throws IOException {
        Order order = createOrder();
        api.placeOrder(order).execute();
        Order fetched = api.getOrderById(order.getId()).execute().body();
        assertEquals(order.getId(), fetched.getId());
        assertEquals(order.getPetId(), fetched.getPetId());
        assertEquals(order.getQuantity(), fetched.getQuantity());
        assertEquals(order.getShipDate().toInstant(), fetched.getShipDate().toInstant());
    }

    private Order createOrder() {
        Order order = new Order();
        order.setPetId(200L);
        order.setQuantity(13);
        //Ensure 3 fractional digits because of a bug in the petstore server
        order.setShipDate(OffsetDateTime.now().withNano(123000000));
        order.setStatus(Order.StatusEnum.PLACED);
        order.setComplete(true);

        try {
            Field idField = Order.class.getDeclaredField("id");
            idField.setAccessible(true);
            idField.set(order, nextId());
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        return order;
    }

    private static final AtomicLong atomicId = createAtomicId();

    private static long nextId() {
        return atomicId.getAndIncrement();
    }

    private static AtomicLong createAtomicId() {
        int baseId = new Random(System.currentTimeMillis()).nextInt(1000000) + 20000;
        return new AtomicLong((long) baseId);
    }
}
