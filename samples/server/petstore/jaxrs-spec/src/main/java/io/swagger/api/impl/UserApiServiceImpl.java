package io.swagger.api.impl;

import java.util.List;
import io.swagger.model.User;

import io.swagger.api.UserApi;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;

import java.util.List;

@Path("/user")

@Api(description = "the user API")




public class UserApiServiceImpl implements UserApi {

    public Response createUser(User body) {
        return Response.ok().entity("magic!").build();
    }

    public Response createUsersWithArrayInput(List<User> body) {
        return Response.ok().entity("magic!").build();
    }

    public Response createUsersWithListInput(List<User> body) {
        return Response.ok().entity("magic!").build();
    }

    public Response deleteUser(String username) {
        return Response.ok().entity("magic!").build();
    }

    public Response getUserByName(String username) {
        return Response.ok().entity("magic!").build();
    }

    public Response loginUser(String username, String password) {
        return Response.ok().entity("magic!").build();
    }

    public Response logoutUser() {
        return Response.ok().entity("magic!").build();
    }

    public Response updateUser(String username, User body) {
        return Response.ok().entity("magic!").build();
    }
}

