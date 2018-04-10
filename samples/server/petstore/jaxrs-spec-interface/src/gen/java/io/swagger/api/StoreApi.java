package io.swagger.api;

import java.util.Map;
import io.swagger.model.Order;


import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/store")
@Api(description = "the store API")

@javax.annotation.Generated(value = "io.swagger.codegen.languages.java.JavaJAXRSSpecServerCodegen", date = "2018-04-04T20:14:19.902+02:00[Europe/Zurich]")
public interface StoreApi {



    @DELETE
    @Path("/order/{order_id}")
    @ApiOperation(value = "Delete purchase order by ID", notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Order not found", response = Void.class) })
    void deleteOrder(@PathParam("order_id") @ApiParam("ID of the order that needs to be deleted") String orderId);


    @GET
    @Path("/inventory")
    @ApiOperation(value = "Returns pet inventories by status", notes = "Returns a map of status codes to quantities", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Map.class, responseContainer = "Map") })
    Map<String, Integer> getInventory();


    @GET
    @Path("/order/{order_id}")
    @ApiOperation(value = "Find purchase order by ID", notes = "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Order not found", response = Void.class) })
    Order getOrderById(@PathParam("order_id") @DecimalMin("1") @DecimalMax("5") @ApiParam("ID of pet that needs to be fetched") Integer orderId);


    @POST
    @Path("/order")
    @ApiOperation(value = "Place an order for a pet", notes = "", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid Order", response = Void.class) })
    Order placeOrder(@Valid Order order);

}
