package io.swagger.api.impl;

import io.swagger.api.*;
import io.swagger.model.Body1;
import io.swagger.model.Body2;
import io.swagger.model.InlineResponse200;
import io.swagger.model.InlineResponse2001;
import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;

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
 * <p>This is a sample Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/). 
 *
 */
public class PetApiServiceImpl implements PetApi {
    /**
     * Add a new parrow to the store
     *
     */
    public InlineResponse2001 addParrot(Body2 body) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Add a new pet to the store
     *
     */
    public void addPet(Pet body) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Deletes a pet
     *
     */
    public void deletePet(Long petId, String apiKey) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Find pet by ID
     *
     * schedule pet feeding
     *
     */
    public void feedPet(Pet body, String token, String petType, String status, Long petId, ) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     *
     */
    public List<Pet> findPetsByStatus(List<String> status) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Finds Pets by tags
     *
     * Muliple tags can be provided with comma separated strings. Use\\ \\ tag1, tag2, tag3 for testing.
     *
     */
    public List<Pet> findPetsByTags(List<String> tags) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * get Parrots
     *
     */
    public List<Object> getParrots() {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Find pet by ID
     *
     * Returns a single pet
     *
     */
    public Pet getPetById(Long petId) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * update parrots
     *
     */
    public InlineResponse200 updateParrots(Body1 body) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Update an existing pet
     *
     */
    public void updatePet(Pet body) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Updates a pet in the store with form data
     *
     */
    public void updatePetWithForm(Long petId, String name, String status) {
        // TODO: Implement...
        
        
    }
    
    /**
     * uploads an image
     *
     */
    public ModelApiResponse uploadFile(Long petId, Object body) {
        // TODO: Implement...
        
        return null;
    }
    
}

