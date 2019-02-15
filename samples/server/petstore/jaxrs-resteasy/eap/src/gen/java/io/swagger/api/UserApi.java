package io.swagger.api;

import io.swagger.model.*;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import io.swagger.model.User;

import java.util.List;
import java.util.Map;

import java.io.InputStream;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.validation.constraints.*;
@Path("/user")


public interface UserApi  {
   
    @POST
    
    @Consumes({ "*/*" })
    
    @Operation(summary = "Create user", description = "This can only be done by the logged in user.", tags={  })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation")
         })
    Response createUser(@Parameter(description = "Created user object" ,required=true) User body,@Context SecurityContext securityContext);

    @POST
    @Path("/createWithArray")
    @Consumes({ "*/*" })
    
    @Operation(summary = "Creates list of users with given input array", description = "", tags={  })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation")
         })
    Response createUsersWithArrayInput(@Parameter(description = "List of user object" ,required=true) List<User> body,@Context SecurityContext securityContext);

    @POST
    @Path("/createWithList")
    @Consumes({ "*/*" })
    
    @Operation(summary = "Creates list of users with given input array", description = "", tags={  })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation")
         })
    Response createUsersWithListInput(@Parameter(description = "List of user object" ,required=true) List<User> body,@Context SecurityContext securityContext);

    @DELETE
    @Path("/{username}")
    
    
    @Operation(summary = "Delete user", description = "This can only be done by the logged in user.", tags={  })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
                @ApiResponse(responseCode = "404", description = "User not found")
         })
    Response deleteUser( @PathParam("username") String username,@Context SecurityContext securityContext);

    @GET
    @Path("/{username}")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Get user by user name", description = "", tags={  })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = User.class))),
                @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
                @ApiResponse(responseCode = "404", description = "User not found")
         })
    Response getUserByName( @PathParam("username") String username,@Context SecurityContext securityContext);

    @GET
    @Path("/login")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Logs user into the system", description = "", tags={  })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = String.class))),
                @ApiResponse(responseCode = "400", description = "Invalid username/password supplied")
         })
    Response loginUser( @NotNull @QueryParam("username") String username, @NotNull @QueryParam("password") String password,@Context SecurityContext securityContext);

    @GET
    @Path("/logout")
    
    
    @Operation(summary = "Logs out current logged in user session", description = "", tags={  })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation")
         })
    Response logoutUser(@Context SecurityContext securityContext);

    @PUT
    @Path("/{username}")
    @Consumes({ "*/*" })
    
    @Operation(summary = "Updated user", description = "This can only be done by the logged in user.", tags={  })
    @ApiResponses(value = {
        @ApiResponse(responseCode = "400", description = "Invalid user supplied"),
                @ApiResponse(responseCode = "404", description = "User not found")
         })
    Response updateUser(@Parameter(description = "Updated user object" ,required=true) User body, @PathParam("username") String username,@Context SecurityContext securityContext);

}
