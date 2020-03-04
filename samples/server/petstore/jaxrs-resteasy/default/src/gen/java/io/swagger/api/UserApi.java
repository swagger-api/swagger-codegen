package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.UserApiService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import io.swagger.model.User;

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
@Path("/user")


public class UserApi  {

    @Inject UserApiService service;

    @POST
    
    @Consumes({ "application/json" })
    
    @Operation(summary = "Create user", description = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public Response createUser(@Parameter(description = "Created user object" ,required=true) User body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.createUser(body,securityContext);
    }
    @POST
    @Path("/createWithArray")
    @Consumes({ "application/json" })
    
    @Operation(summary = "Creates list of users with given input array", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public Response createUsersWithArrayInput(@Parameter(description = "List of user object" ,required=true) List<User> body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.createUsersWithArrayInput(body,securityContext);
    }
    @POST
    @Path("/createWithList")
    @Consumes({ "application/json" })
    
    @Operation(summary = "Creates list of users with given input array", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public Response createUsersWithListInput(@Parameter(description = "List of user object" ,required=true) List<User> body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.createUsersWithListInput(body,securityContext);
    }
    @DELETE
    @Path("/{username}")
    
    
    @Operation(summary = "Delete user", description = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        
        @ApiResponse(responseCode = "404", description = "User not found") })
    public Response deleteUser( @PathParam("username") String username,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.deleteUser(username,securityContext);
    }
    @GET
    @Path("/{username}")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Get user by user name", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = User.class))),
        
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        
        @ApiResponse(responseCode = "404", description = "User not found") })
    public Response getUserByName( @PathParam("username") String username,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.getUserByName(username,securityContext);
    }
    @GET
    @Path("/login")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Logs user into the system", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = String.class))),
        
        @ApiResponse(responseCode = "400", description = "Invalid username/password supplied") })
    public Response loginUser( @NotNull  @QueryParam("username") String username, @NotNull  @QueryParam("password") String password,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.loginUser(username,password,securityContext);
    }
    @GET
    @Path("/logout")
    
    
    @Operation(summary = "Logs out current logged in user session", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public Response logoutUser(@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.logoutUser(securityContext);
    }
    @PUT
    @Path("/{username}")
    @Consumes({ "application/json" })
    
    @Operation(summary = "Updated user", description = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid user supplied"),
        
        @ApiResponse(responseCode = "404", description = "User not found") })
    public Response userUsernamePut(@Parameter(description = "Updated user object" ,required=true) User body, @PathParam("username") String username,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.userUsernamePut(body,username,securityContext);
    }
}
