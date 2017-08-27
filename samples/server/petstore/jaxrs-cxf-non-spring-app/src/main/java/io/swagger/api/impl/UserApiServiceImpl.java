package io.swagger.api.impl;

import io.swagger.api.*;
import java.util.List;
import io.swagger.model.User;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import org.apache.cxf.jaxrs.model.wadl.Description;
import org.apache.cxf.jaxrs.model.wadl.DocTarget;

import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;

public class UserApiServiceImpl implements UserApi {
    public Void createUser(User body) {
        // TODO: Implement...
        
        
    }
    
    public Void createUsersWithArrayInput(List<User> body) {
        // TODO: Implement...
        
        
    }
    
    public Void createUsersWithListInput(List<User> body) {
        // TODO: Implement...
        
        
    }
    
    public Void deleteUser(String username) {
        // TODO: Implement...
        
        
    }
    
    public User getUserByName(String username) {
        // TODO: Implement...
        
        return null;
    }
    
    public String loginUser(String username, String password) {
        // TODO: Implement...
        
        return null;
    }
    
    public Void logoutUser() {
        // TODO: Implement...
        
        
    }
    
    public Void updateUser(String username, User body) {
        // TODO: Implement...
        
        
    }
    
}

