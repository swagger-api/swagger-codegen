package io.swagger.api.impl;

import io.swagger.api.*;
import io.swagger.model.Animal;

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
public class AnimalApiServiceImpl implements AnimalApi {
    /**
     * Add a new animal to the store
     *
     */
    public void addAnimal(Animal body) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Deletes a animal
     *
     */
    public void deleteAnimal(Long animalId, String apiKey) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Find animal by ID
     *
     * Returns a single animal
     *
     */
    public Animal getAnimalById(Long animalId) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Update an existing animal
     *
     */
    public void updateAnimal(Animal body) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Updates a animal
     *
     */
    public void updateAnimalWithForm(Long animalId, String name, String status) {
        // TODO: Implement...
        
        
    }
    
}

