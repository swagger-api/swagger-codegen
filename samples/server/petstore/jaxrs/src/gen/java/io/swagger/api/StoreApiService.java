package io.swagger.api;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

import io.swagger.model.Order;


public abstract class StoreApiService {
    public abstract Response deleteOrder(String orderId, SecurityContext securityContext)
            throws NotFoundException;

    public abstract Response getInventory(SecurityContext securityContext)
            throws NotFoundException;

    public abstract Response getOrderById(Long orderId, SecurityContext securityContext)
            throws NotFoundException;

    public abstract Response placeOrder(Order body, SecurityContext securityContext)
            throws NotFoundException;
}
