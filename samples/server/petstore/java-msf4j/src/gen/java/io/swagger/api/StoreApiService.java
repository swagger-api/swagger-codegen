package io.swagger.api;

import javax.ws.rs.core.Response;

import io.swagger.api.*;
import io.swagger.api.NotFoundException;
import io.swagger.model.Order;


public abstract class StoreApiService {
    public abstract Response deleteOrder(String orderId ) throws NotFoundException;
    public abstract Response getInventory() throws NotFoundException;
    public abstract Response getOrderById(Long orderId ) throws NotFoundException;
    public abstract Response placeOrder(Order body ) throws NotFoundException;
}
