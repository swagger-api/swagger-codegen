package io.swagger.api.impl;

import io.swagger.api.*;
import java.io.File;
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

import io.swagger.annotations.Api;

public class PetApiServiceImpl implements PetApi {
    public Response addPet(Pet body) {
        // TODO: Implement...
        
        
    }
    
    public Response deletePet(Long petId, String apiKey) {
        // TODO: Implement...
        
        
    }
    
    public Response findPetsByStatus(List<String> status) {
        // TODO: Implement...
        
        return null;
    }
    
    public Response findPetsByTags(List<String> tags) {
        // TODO: Implement...
        
        return null;
    }
    
    public Response getPetById(Long petId) {
        // TODO: Implement...
        
        return null;
    }
    
    public Response updatePet(Pet body) {
        // TODO: Implement...
        
        
    }
    
    public Response updatePetWithForm(Long petId, String name, String status) {
        // TODO: Implement...
        
        
    }
    
    public Response uploadFile(Long petId, String additionalMetadata,  Attachment fileDetail) {
        // TODO: Implement...
        
        return null;
    }
    
}

