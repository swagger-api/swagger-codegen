package io.swagger.client.apis

import io.swagger.client.infrastructure.ApiClient
import io.swagger.client.models.ApiResponse
import io.swagger.client.models.Pet
import org.junit.Assert
import org.junit.Before
import org.junit.Test

/**
 * API tests for PetApi
 */
class PetApiTest {

    private lateinit var api: PetApi

    @Before
    fun setup() {
        api = ApiClient().createService(PetApi::class.java)
    }

    /**
     * Add a new pet to the store
     *
     * 
     */
    @Test
    fun addPetTest() {
        val body: Pet? = null
        // val response = api.addPet(body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Deletes a pet
     *
     * 
     */
    @Test
    fun deletePetTest() {
        val petId: kotlin.Long? = null
        val apiKey: kotlin.String? = null
        // val response = api.deletePet(petId, apiKey).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     */
    @Test
    fun findPetsByStatusTest() {
        val status: kotlin.collections.List<kotlin.String>? = null
        // val response = api.findPetsByStatus(status).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Finds Pets by tags
     *
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     */
    @Test
    fun findPetsByTagsTest() {
        val tags: kotlin.collections.List<kotlin.String>? = null
        // val response = api.findPetsByTags(tags).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Find pet by ID
     *
     * Returns a single pet
     */
    @Test
    fun getPetByIdTest() {
        val petId: kotlin.Long? = null
        // val response = api.getPetById(petId).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Update an existing pet
     *
     * 
     */
    @Test
    fun updatePetTest() {
        val body: Pet? = null
        // val response = api.updatePet(body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Updates a pet in the store with form data
     *
     * 
     */
    @Test
    fun updatePetWithFormTest() {
        val petId: kotlin.Long? = null
        val name: kotlin.String? = null
        val status: kotlin.String? = null
        // val response = api.updatePetWithForm(petId, name, status).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * uploads an image
     *
     * 
     */
    @Test
    fun uploadFileTest() {
        val petId: kotlin.Long? = null
        val additionalMetadata: kotlin.String? = null
        val file: java.io.File? = null
        // val response = api.uploadFile(petId, additionalMetadata, file).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }
}
