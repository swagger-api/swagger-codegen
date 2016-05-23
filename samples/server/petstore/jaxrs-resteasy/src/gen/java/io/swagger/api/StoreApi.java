package io.swagger.api;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

import io.swagger.api.factories.StoreApiServiceFactory;
import io.swagger.model.Order;

@Path("/store")


@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-04-29T00:20:47.240+08:00")
public class StoreApi {
    private final StoreApiService delegate = StoreApiServiceFactory.getStoreApi();

    @DELETE
    @Path("/order/{orderId}")

    @Produces({"application/xml", "application/json"})
    public Response deleteOrder(@PathParam("orderId") String orderId, @Context SecurityContext securityContext)
            throws NotFoundException {
        return delegate.deleteOrder(orderId, securityContext);
    }

    @GET
    @Path("/inventory")

    @Produces({"application/json"})
    public Response getInventory(@Context SecurityContext securityContext)
            throws NotFoundException {
        return delegate.getInventory(securityContext);
    }

    @GET
    @Path("/order/{orderId}")

    @Produces({"application/xml", "application/json"})
    public Response getOrderById(@PathParam("orderId") Long orderId, @Context SecurityContext securityContext)
            throws NotFoundException {
        return delegate.getOrderById(orderId, securityContext);
    }

    @POST
    @Path("/order")

    @Produces({"application/xml", "application/json"})
    public Response placeOrder(Order body, @Context SecurityContext securityContext)
            throws NotFoundException {
        return delegate.placeOrder(body, securityContext);
    }
}
