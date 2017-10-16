package io.swagger.api;

import io.swagger.model.User;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ApiResponse;
import io.swagger.jaxrs.PATCH;

@Path("/")
@Api(value = "/", description = "")
public interface UserApi  {

    @POST
    @Path("/user")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Create user", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 0, message = "successful operation", response = .class) })
    public void createUser(User body);

    @POST
    @Path("/user/createWithArray")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Creates list of users with given input array", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 0, message = "successful operation", response = .class) })
    public void createUsersWithArrayInput(List<User> body);

    @POST
    @Path("/user/createWithList")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Creates list of users with given input array", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 0, message = "successful operation", response = .class) })
    public void createUsersWithListInput(List<User> body);

    @DELETE
    @Path("/user/{username}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Delete user", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = .class),
        @ApiResponse(code = 404, message = "User not found", response = .class) })
    public void deleteUser(@PathParam("username") String username);

    @GET
    @Path("/user/{username}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Get user by user name", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = User.class),
        @ApiResponse(code = 400, message = "Invalid username supplied", response = .class),
        @ApiResponse(code = 404, message = "User not found", response = .class) })
    public User getUserByName(@PathParam("username") String username);

    @GET
    @Path("/user/login")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Logs user into the system", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = String.class),
        @ApiResponse(code = 400, message = "Invalid username/password supplied", response = .class) })
    public String loginUser(@QueryParam("username")String username, @QueryParam("password")String password);

    @GET
    @Path("/user/logout")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Logs out current logged in user session", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 0, message = "successful operation", response = .class) })
    public void logoutUser();

    @PUT
    @Path("/user/{username}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Updated user", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid user supplied", response = .class),
        @ApiResponse(code = 404, message = "User not found", response = .class) })
    public void updateUser(@PathParam("username") String username, User body);
}

