package io.swagger.client.api.rxjava;

import io.swagger.client.model.ModelApiResponse;
import io.swagger.client.model.Pet;


import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;



public class PetApi {

	private final io.swagger.client.api.PetApi delegate;

	public PetApi(io.swagger.client.api.PetApi delegate) {
	    this.delegate = delegate;
    }

	public io.swagger.client.api.PetApi getDelegate() {
	    return delegate;
	}

    
    /**
     * Add a new pet to the store
     * 
     
     * @param pet Pet object that needs to be added to the store (required)
     
     * @param resultHandler Asynchronous result handler
     */
    public void addPet(Pet pet, Handler<AsyncResult<Void>> resultHandler) {
        delegate.addPet(pet, resultHandler);
    }

    /**
     * Add a new pet to the store
     * 
     
     * @param pet Pet object that needs to be added to the store (required)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxAddPet(Pet pet) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.addPet(pet, fut);
        }));
    }
    
    /**
     * Deletes a pet
     * 
     
     * @param petId Pet id to delete (required)
     
     * @param apiKey  (optional)
     
     * @param resultHandler Asynchronous result handler
     */
    public void deletePet(Integer petId, String apiKey, Handler<AsyncResult<Void>> resultHandler) {
        delegate.deletePet(petId, apiKey, resultHandler);
    }

    /**
     * Deletes a pet
     * 
     
     * @param petId Pet id to delete (required)
     
     * @param apiKey  (optional)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxDeletePet(Integer petId, String apiKey) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.deletePet(petId, apiKey, fut);
        }));
    }
    
    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     
     * @param status Status values that need to be considered for filter (required)
     
     * @param resultHandler Asynchronous result handler
     */
    public void findPetsByStatus(List<String> status, Handler<AsyncResult<List<Pet>>> resultHandler) {
        delegate.findPetsByStatus(status, resultHandler);
    }

    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     
     * @param status Status values that need to be considered for filter (required)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<List<Pet>> rxFindPetsByStatus(List<String> status) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.findPetsByStatus(status, fut);
        }));
    }
    
    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     
     * @param tags Tags to filter by (required)
     
     * @param resultHandler Asynchronous result handler
     */
    public void findPetsByTags(List<String> tags, Handler<AsyncResult<List<Pet>>> resultHandler) {
        delegate.findPetsByTags(tags, resultHandler);
    }

    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     
     * @param tags Tags to filter by (required)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<List<Pet>> rxFindPetsByTags(List<String> tags) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.findPetsByTags(tags, fut);
        }));
    }
    
    /**
     * Find pet by ID
     * Returns a single pet
     
     * @param petId ID of pet to return (required)
     
     * @param resultHandler Asynchronous result handler
     */
    public void getPetById(Integer petId, Handler<AsyncResult<Pet>> resultHandler) {
        delegate.getPetById(petId, resultHandler);
    }

    /**
     * Find pet by ID
     * Returns a single pet
     
     * @param petId ID of pet to return (required)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Pet> rxGetPetById(Integer petId) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.getPetById(petId, fut);
        }));
    }
    
    /**
     * Update an existing pet
     * 
     
     * @param pet Pet object that needs to be added to the store (required)
     
     * @param resultHandler Asynchronous result handler
     */
    public void updatePet(Pet pet, Handler<AsyncResult<Void>> resultHandler) {
        delegate.updatePet(pet, resultHandler);
    }

    /**
     * Update an existing pet
     * 
     
     * @param pet Pet object that needs to be added to the store (required)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxUpdatePet(Pet pet) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.updatePet(pet, fut);
        }));
    }
    
    /**
     * Updates a pet in the store with form data
     * 
     
     * @param petId ID of pet that needs to be updated (required)
     
     * @param body  (optional)
     
     * @param resultHandler Asynchronous result handler
     */
    public void updatePetWithForm(Integer petId, Object body, Handler<AsyncResult<Void>> resultHandler) {
        delegate.updatePetWithForm(petId, body, resultHandler);
    }

    /**
     * Updates a pet in the store with form data
     * 
     
     * @param petId ID of pet that needs to be updated (required)
     
     * @param body  (optional)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxUpdatePetWithForm(Integer petId, Object body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.updatePetWithForm(petId, body, fut);
        }));
    }
    
    /**
     * uploads an image
     * 
     
     * @param petId ID of pet to update (required)
     
     * @param body  (optional)
     
     * @param resultHandler Asynchronous result handler
     */
    public void uploadFile(Integer petId, Object body, Handler<AsyncResult<ModelApiResponse>> resultHandler) {
        delegate.uploadFile(petId, body, resultHandler);
    }

    /**
     * uploads an image
     * 
     
     * @param petId ID of pet to update (required)
     
     * @param body  (optional)
     
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<ModelApiResponse> rxUploadFile(Integer petId, Object body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.uploadFile(petId, body, fut);
        }));
    }
    

    public static PetApi newInstance(io.swagger.client.api.PetApi arg) {
        return arg != null ? new PetApi(arg) : null;
    }
}

