package io.swagger.api;

import io.swagger.model.User;

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

@Path("/user")

@javax.annotation.Generated(value = "io.swagger.codegen.v3.generators.java.JavaJAXRSSpecServerCodegen", date = "2020-02-20T00:57:07.355-05:00[America/Bogota]")
public interface UserApi {

    @POST
    @Consumes({ "application/json" })
    @Operation(summary = "Create user", description = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    void createUser(@Valid User body);
    @POST
    @Path("/createWithArray")
    @Consumes({ "application/json" })
    @Operation(summary = "Creates list of users with given input array", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    void createUsersWithArrayInput(@Valid List<User> body);
    @POST
    @Path("/createWithList")
    @Consumes({ "application/json" })
    @Operation(summary = "Creates list of users with given input array", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    void createUsersWithListInput(@Valid List<User> body);
    @DELETE
    @Path("/{username}")
    @Operation(summary = "Delete user", description = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        @ApiResponse(responseCode = "404", description = "User not found") })
    void deleteUser( @PathParam("username")

 @Parameter(description = "The name that needs to be deleted") String username
);
    @GET
    @Path("/{username}")
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Get user by user name", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = User.class))),
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        @ApiResponse(responseCode = "404", description = "User not found") })
    User getUserByName( @PathParam("username")

 @Parameter(description = "The name that needs to be fetched. Use user1 for testing.") String username
);
    @GET
    @Path("/login")
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Logs user into the system", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = String.class))),
        @ApiResponse(responseCode = "400", description = "Invalid username/password supplied") })
    String loginUser( @NotNull  @QueryParam("username") 

 @Parameter(description = "The user name for login")  String username
, @NotNull  @QueryParam("password") 

 @Parameter(description = "The password for login in clear text")  String password
);
    @GET
    @Path("/logout")
    @Operation(summary = "Logs out current logged in user session", description = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    void logoutUser();
    @PUT
    @Path("/{username}")
    @Consumes({ "application/json" })
    @Operation(summary = "Updated user", description = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid user supplied"),
        @ApiResponse(responseCode = "404", description = "User not found") })
    void userUsernamePut(@Valid User body, @PathParam("username")

 @Parameter(description = "name that need to be updated") String username
);}
