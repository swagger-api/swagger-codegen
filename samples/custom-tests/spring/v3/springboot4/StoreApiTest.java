package io.swagger.api;

import io.swagger.model.Order;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

@SpringBootTest
public class StoreApiTest {

    @Autowired
    private StoreApi api;

    @Test
    public void deleteOrderTest() throws Exception {
        Long orderId = 789L;
        ResponseEntity<Void> responseEntity = api.deleteOrder(orderId);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void getInventoryTest() throws Exception {
        ResponseEntity<Map<String, Integer>> responseEntity = api.getInventory();
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void getOrderByIdTest() throws Exception {
        Long orderId = 789L;
        ResponseEntity<Order> responseEntity = api.getOrderById(orderId);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void placeOrderTest() throws Exception {
        Order body = new Order();
        ResponseEntity<Order> responseEntity = api.placeOrder(body);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

}
