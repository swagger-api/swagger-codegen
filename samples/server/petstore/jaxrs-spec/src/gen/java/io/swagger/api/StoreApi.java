package io.swagger.api;

import java.util.Map;
import io.swagger.model.Order;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;

import java.util.List;
import javax.validation.constraints.*;

@Path("/store")

@Api(description = "the store API")




public interface StoreApi  {

    @DELETE
    @Path("/order/{orderId}")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Delete purchase order by ID", notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors", tags={ "store",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied"),
        @ApiResponse(code = 404, message = "Order not found") })
    public Response deleteOrder(@PathParam("orderId") @ApiParam("ID of the order that needs to be deleted") String orderId);

    @GET
    @Path("/inventory")
    
    @Produces({ "application/json" })
    @ApiOperation(value = "Returns pet inventories by status", notes = "Returns a map of status codes to quantities", response = Integer.class, responseContainer = "Map", authorizations = {
        @Authorization(value = "api_key")
    }, tags={ "store",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Map.class, responseContainer = "Map") })
    public Response getInventory();

    @GET
    @Path("/order/{orderId}")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Find purchase order by ID", notes = "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions", response = Order.class, tags={ "store",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied"),
        @ApiResponse(code = 404, message = "Order not found") })
    public Response getOrderById(@PathParam("orderId") @Min(1) @Max(5) @ApiParam("ID of pet that needs to be fetched") Long orderId);

    @POST
    @Path("/order")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Place an order for a pet", notes = "", response = Order.class, tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid Order") })
    public Response placeOrder(Order body);
}

