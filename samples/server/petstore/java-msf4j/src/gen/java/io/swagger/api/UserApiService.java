package io.swagger.api;

import java.util.List;

import javax.ws.rs.core.Response;

import io.swagger.api.*;
import io.swagger.api.NotFoundException;
import io.swagger.model.User;


public abstract class UserApiService {
    public abstract Response createUser(User body ) throws NotFoundException;
    public abstract Response createUsersWithArrayInput(List<User> body ) throws NotFoundException;
    public abstract Response createUsersWithListInput(List<User> body ) throws NotFoundException;
    public abstract Response deleteUser(String username ) throws NotFoundException;
    public abstract Response getUserByName(String username ) throws NotFoundException;
    public abstract Response loginUser(String username ,String password ) throws NotFoundException;
    public abstract Response logoutUser() throws NotFoundException;
    public abstract Response updateUser(String username ,User body ) throws NotFoundException;
}
