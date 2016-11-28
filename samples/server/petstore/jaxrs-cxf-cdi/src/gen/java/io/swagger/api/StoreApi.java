package io.swagger.api;

import javax.ws.rs.core.Response;

import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.model.Order;

@Path("/v2")
public interface StoreApi  {
    @DELETE
    @Path("/order/{orderId}")
    
    @Produces({ "application/xml", "application/json" })
    Response deleteOrder(@PathParam("orderId") String orderId);
    @GET
    @Path("/inventory")
    
    @Produces({ "application/json" })
    Response getInventory();
    @GET
    @Path("/order/{orderId}")
    
    @Produces({ "application/xml", "application/json" })
    Response getOrderById(@PathParam("orderId") Long orderId);
    @POST
    @Path("/order")
    
    @Produces({ "application/xml", "application/json" })
    Response placeOrder(Order body);
}

