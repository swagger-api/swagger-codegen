package io.swagger.client.api;

import io.swagger.client.ApiClient;
import io.swagger.client.model.Animal;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for AnimalApi
 */
public class AnimalApiTest {

    private AnimalApi api;

    @Before
    public void setup() {
        api = new ApiClient().createService(AnimalApi.class);
    }


    /**
     * Add a new animal to the store
     *
     * 
     */
    @Test
    public void addAnimalTest() {
        Animal body = null;
        // Void response = api.addAnimal(body);

        // TODO: test validations
    }

    /**
     * Deletes a animal
     *
     * 
     */
    @Test
    public void deleteAnimalTest() {
        Long animalId = null;
        String apiKey = null;
        // Void response = api.deleteAnimal(animalId, apiKey);

        // TODO: test validations
    }

    /**
     * Find animal by ID
     *
     * Returns a single animal
     */
    @Test
    public void getAnimalByIdTest() {
        Long animalId = null;
        // Animal response = api.getAnimalById(animalId);

        // TODO: test validations
    }

    /**
     * Update an existing animal
     *
     * 
     */
    @Test
    public void updateAnimalTest() {
        Animal body = null;
        // Void response = api.updateAnimal(body);

        // TODO: test validations
    }

    /**
     * Updates a animal
     *
     * 
     */
    @Test
    public void updateAnimalWithFormTest() {
        Long animalId = null;
        String name = null;
        String status = null;
        // Void response = api.updateAnimalWithForm(animalId, name, status);

        // TODO: test validations
    }
}
