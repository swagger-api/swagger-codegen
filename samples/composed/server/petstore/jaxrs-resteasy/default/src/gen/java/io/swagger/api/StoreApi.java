package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.StoreApiService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import java.util.Map;
import io.swagger.model.Order;

import java.util.Map;
import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.inject.Inject;

import javax.validation.constraints.*;
@Path("/store")


public class StoreApi  {

    @Inject StoreApiService service;

    @DELETE
    @Path("/order/{orderId}")
    
    
    @Operation(summary = "Delete purchase order by ID", description = "For valid response try integer IDs with positive integer value.\\ \\ Negative or non-integer values will generate API errors", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        
        @ApiResponse(responseCode = "404", description = "Order not found") })
    public Response deleteOrder( @Min(1L) @PathParam("orderId") Long orderId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.deleteOrder(orderId,securityContext);
    }
    @GET
    @Path("/inventory")
    
    @Produces({ "application/json" })
    @Operation(summary = "Returns pet inventories by status", description = "Returns a map of status codes to quantities", security = {
        @SecurityRequirement(name = "api_key")
    }, tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Map.class)))) })
    public Response getInventory(@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.getInventory(securityContext);
    }
    @GET
    @Path("/order/{orderId}")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Find purchase order by ID", description = "For valid response try integer IDs with value >= 1 and <= 10.\\ \\ Other values will generated exceptions", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Order.class))),
        
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        
        @ApiResponse(responseCode = "404", description = "Order not found") })
    public Response getOrderById( @Min(1L) @Max(10L) @PathParam("orderId") Long orderId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.getOrderById(orderId,securityContext);
    }
    @POST
    @Path("/order")
    @Consumes({ "application/json" })
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Place an order for a pet", description = "", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Order.class))),
        
        @ApiResponse(responseCode = "400", description = "Invalid Order") })
    public Response placeOrder(@Parameter(description = "order placed for purchasing the pet" ,required=true) Order body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.placeOrder(body,securityContext);
    }
}
