package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.InventoryApiService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import io.swagger.model.ClassAorB;

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
@Path("/inventory")


@javax.annotation.Generated(value = "io.swagger.codegen.v3.generators.java.JavaResteasyServerCodegen", date = "2022-07-13T13:08:29.879399+02:00[Europe/Rome]")public class InventoryApi  {

    @Inject InventoryApiService service = new io.swagger.api.impl.InventoryApiServiceImpl();

    @POST
    
    @Consumes({ "application/json" })
    
    @Operation(summary = "adds an inventory item", description = "Adds an item to the system", tags={ "admins" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "201", description = "item created"),
        
        @ApiResponse(responseCode = "400", description = "invalid input, object invalid"),
        
        @ApiResponse(responseCode = "409", description = "an existing item already exists") })
    public Response addNewInventory(@Parameter(description = "Inventory item to add" ) ClassAorB body,@Context SecurityContext securityContext)
    throws NotFoundException {
        System.out.println("vod " + body.getClass());
        System.out.println("ClassAorB " + (body instanceof io.swagger.model.ClassAorB));
        System.out.println("ClassA " + (body instanceof io.swagger.model.ClassA));
        System.out.println("ClassB " + (body instanceof io.swagger.model.ClassB));
        System.out.println("OneOfClassAorB " + (body instanceof io.swagger.model.OneOfClassAorB));
        return service.addNewInventory(body,securityContext);
    }
}
