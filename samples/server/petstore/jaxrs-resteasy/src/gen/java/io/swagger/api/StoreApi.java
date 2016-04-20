package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.StoreApiService;
import io.swagger.api.factories.StoreApiServiceFactory;

import io.swagger.model.Order;
<<<<<<< HEAD
import java.util.Map;
=======
>>>>>>> upstream/master

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;

@Path("/store")


<<<<<<< HEAD
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-03-16T14:27:58.108+08:00")
=======
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-04-15T18:10:39.667+08:00")
>>>>>>> upstream/master
public class StoreApi  {
   private final StoreApiService delegate = StoreApiServiceFactory.getStoreApi();

    @DELETE
    @Path("/order/{orderId}")
    
<<<<<<< HEAD
    @Produces({ "application/json", "application/xml" })
=======
    @Produces({ "application/xml", "application/json" })
>>>>>>> upstream/master
    public Response deleteOrder( @PathParam("orderId") String orderId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deleteOrder(orderId,securityContext);
    }
<<<<<<< HEAD
    @GET
    @Path("/findByStatus")
    
    @Produces({ "application/json", "application/xml" })
    public Response findOrdersByStatus( @QueryParam("status") String status,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findOrdersByStatus(status,securityContext);
    }
=======
>>>>>>> upstream/master
    @GET
    @Path("/inventory")
    
    @Produces({ "application/json" })
    public Response getInventory(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getInventory(securityContext);
    }
<<<<<<< HEAD
    @GET
    @Path("/inventory?response=arbitrary_object")
    
    @Produces({ "application/json", "application/xml" })
    public Response getInventoryInObject(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getInventoryInObject(securityContext);
    }
=======
>>>>>>> upstream/master
    @GET
    @Path("/order/{orderId}")
    
    @Produces({ "application/xml", "application/json" })
    public Response getOrderById( @PathParam("orderId") Long orderId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getOrderById(orderId,securityContext);
    }
    @POST
    @Path("/order")
    
<<<<<<< HEAD
    @Produces({ "application/json", "application/xml" })
=======
    @Produces({ "application/xml", "application/json" })
>>>>>>> upstream/master
    public Response placeOrder( Order body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.placeOrder(body,securityContext);
    }
}
