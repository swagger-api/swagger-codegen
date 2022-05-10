package io.swagger.api.impl;

import io.swagger.api.*;
import io.swagger.model.Dog;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import org.apache.cxf.jaxrs.model.wadl.Description;
import org.apache.cxf.jaxrs.model.wadl.DocTarget;

import org.apache.cxf.jaxrs.ext.multipart.*;


/**
 * Swagger Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
public class DogApiServiceImpl implements DogApi {
    /**
     * Add a new dog to the store
     *
     */
    public void addDog(Dog body) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Deletes a dog
     *
     */
    public void deleteDog(Long dogId, String apiKey) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Find dog by ID
     *
     * Returns a single dog
     *
     */
    public Dog getDogById(Long dogId) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Update an existing dog
     *
     */
    public void updateDog(Dog body) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Updates a dog
     *
     */
    public void updateDogWithForm(Long dogId, String name, String status) {
        // TODO: Implement...
        
        
    }
    
}

