package io.swagger.api;

import java.util.Map;
import io.swagger.model.Order;

import java.util.*;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;

import static org.junit.Assert.assertEquals;

@RunWith(SpringRunner.class)
@SpringBootTest
public class StoreApiControllerIntegrationTest {

    @Autowired
    private StoreApi api;

    @Test
    public void deleteOrderTest() throws Exception {
        ResponseEntity<Void> responseEntity = api.deleteOrder(10L);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void getInventoryTest() throws Exception {
        ResponseEntity<Map<String, Integer>> responseEntity = api.getInventory();
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void getOrderByIdTest() throws Exception {
        Long orderId = 56L;
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
