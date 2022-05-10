package io.swagger.api;

import java.util.Map;
import io.swagger.model.Order;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/store")

@javax.annotation.Generated(value = "io.swagger.codegen.v3.generators.java.JavaJAXRSSpecServerCodegen", date = "2020-02-20T00:57:07.355-05:00[America/Bogota]")
public interface StoreApi {

    @DELETE
    @Path("/order/{orderId}")
    @Operation(summary = "Delete purchase order by ID", description = "For valid response try integer IDs with positive integer value.\\ \\ Negative or non-integer values will generate API errors", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Order not found") })
    void deleteOrder( @Min(1L) @PathParam("orderId")

 @Parameter(description = "ID of the order that needs to be deleted") Long orderId
);
    @GET
    @Path("/inventory")
    @Produces({ "application/json" })
    @Operation(summary = "Returns pet inventories by status", description = "Returns a map of status codes to quantities", security = {
        @SecurityRequirement(name = "api_key")    }, tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Map.class)))) })
    Map<String, Integer> getInventory();
    @GET
    @Path("/order/{orderId}")
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Find purchase order by ID", description = "For valid response try integer IDs with value >= 1 and <= 10.\\ \\ Other values will generated exceptions", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Order.class))),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Order not found") })
    Order getOrderById( @Min(1L) @Max(10L) @PathParam("orderId")

 @Parameter(description = "ID of pet that needs to be fetched") Long orderId
);
    @POST
    @Path("/order")
    @Consumes({ "application/json" })
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Place an order for a pet", description = "", tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Order.class))),
        @ApiResponse(responseCode = "400", description = "Invalid Order") })
    Order placeOrder(@Valid Order body);}
