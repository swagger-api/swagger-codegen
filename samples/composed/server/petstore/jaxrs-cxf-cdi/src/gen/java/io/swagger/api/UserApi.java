package io.swagger.api;

import io.swagger.model.User;
import io.swagger.api.UserApiService;

import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import java.io.InputStream;

import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
@Path("/user")
@RequestScoped





public class UserApi  {

  @Context SecurityContext securityContext;

  @Inject UserApiService delegate;


    @POST
    
    @Consumes({ "application/json" })
    
    @Operation(summary = "Create user", description = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public Response createUser(
@Parameter(description = "Created user object" ,required=true) User body
) {
        return delegate.createUser(body, securityContext);
    }

    @POST
    @Path("/createWithArray")
    @Consumes({ "application/json" })
    
    @Operation(summary = "Creates list of users with given input array", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public Response createUsersWithArrayInput(
@Parameter(description = "List of user object" ,required=true) List<User> body
) {
        return delegate.createUsersWithArrayInput(body, securityContext);
    }

    @POST
    @Path("/createWithList")
    @Consumes({ "application/json" })
    
    @Operation(summary = "Creates list of users with given input array", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public Response createUsersWithListInput(
@Parameter(description = "List of user object" ,required=true) List<User> body
) {
        return delegate.createUsersWithListInput(body, securityContext);
    }

    @DELETE
    @Path("/{username}")
    
    
    @Operation(summary = "Delete user", description = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        @ApiResponse(responseCode = "404", description = "User not found") })
    public Response deleteUser(
@Parameter(description = "The name that needs to be deleted",required=true) @PathParam("username") String username
) {
        return delegate.deleteUser(username, securityContext);
    }

    @GET
    @Path("/{username}")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Get user by user name", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = User.class))),
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        @ApiResponse(responseCode = "404", description = "User not found") })
    public Response getUserByName(
@Parameter(description = "The name that needs to be fetched. Use user1 for testing.",required=true) @PathParam("username") String username
) {
        return delegate.getUserByName(username, securityContext);
    }

    @GET
    @Path("/login")
    
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Logs user into the system", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = String.class))),
        @ApiResponse(responseCode = "400", description = "Invalid username/password supplied") })
    public Response loginUser( @NotNull 
@Parameter(description = "The user name for login",required=true)  @QueryParam("username") String username
,  @NotNull 
@Parameter(description = "The password for login in clear text",required=true)  @QueryParam("password") String password
) {
        return delegate.loginUser(username, password, securityContext);
    }

    @GET
    @Path("/logout")
    
    
    @Operation(summary = "Logs out current logged in user session", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public Response logoutUser() {
        return delegate.logoutUser(securityContext);
    }

    @PUT
    @Path("/{username}")
    @Consumes({ "application/json" })
    
    @Operation(summary = "Updated user", description = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid user supplied"),
        @ApiResponse(responseCode = "404", description = "User not found") })
    public Response userUsernamePut(
@Parameter(description = "Updated user object" ,required=true) User body
, 
@Parameter(description = "name that need to be updated",required=true) @PathParam("username") String username
) {
        return delegate.userUsernamePut(body, username, securityContext);
    }
}
