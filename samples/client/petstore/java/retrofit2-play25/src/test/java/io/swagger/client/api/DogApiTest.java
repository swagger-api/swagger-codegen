package io.swagger.client.api;

import io.swagger.client.ApiClient;
import io.swagger.client.model.Dog;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for DogApi
 */
public class DogApiTest {

    private DogApi api;

    @Before
    public void setup() {
        api = new ApiClient().createService(DogApi.class);
    }


    /**
     * Add a new dog to the store
     *
     * 
     */
    @Test
    public void addDogTest() {
        Dog body = null;
        // Void response = api.addDog(body);

        // TODO: test validations
    }

    /**
     * Deletes a dog
     *
     * 
     */
    @Test
    public void deleteDogTest() {
        Long dogId = null;
        String apiKey = null;
        // Void response = api.deleteDog(dogId, apiKey);

        // TODO: test validations
    }

    /**
     * Find dog by ID
     *
     * Returns a single dog
     */
    @Test
    public void getDogByIdTest() {
        Long dogId = null;
        // Dog response = api.getDogById(dogId);

        // TODO: test validations
    }

    /**
     * Update an existing dog
     *
     * 
     */
    @Test
    public void updateDogTest() {
        Dog body = null;
        // Void response = api.updateDog(body);

        // TODO: test validations
    }

    /**
     * Updates a dog
     *
     * 
     */
    @Test
    public void updateDogWithFormTest() {
        Long animalId = null;
        String name = null;
        String status = null;
        // Void response = api.updateDogWithForm(animalId, name, status);

        // TODO: test validations
    }
}
