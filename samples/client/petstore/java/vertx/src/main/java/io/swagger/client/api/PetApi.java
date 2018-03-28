package io.swagger.client.api;

import io.swagger.client.model.ModelApiResponse;
import io.swagger.client.model.Pet;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface PetApi {

    
    
    void addPet(Pet pet, Handler<AsyncResult<Void>> handler);

    
    void deletePet(Integer petId, String apiKey, Handler<AsyncResult<Void>> handler);

    
    void findPetsByStatus(List<String> status, Handler<AsyncResult<List<Pet>>> handler);

    
    void findPetsByTags(List<String> tags, Handler<AsyncResult<List<Pet>>> handler);

    
    void getPetById(Integer petId, Handler<AsyncResult<Pet>> handler);

    
    void updatePet(Pet pet, Handler<AsyncResult<Void>> handler);

    
    void updatePetWithForm(Integer petId, Object body, Handler<AsyncResult<Void>> handler);

    
    void uploadFile(Integer petId, Object body, Handler<AsyncResult<ModelApiResponse>> handler);

    
    
}
