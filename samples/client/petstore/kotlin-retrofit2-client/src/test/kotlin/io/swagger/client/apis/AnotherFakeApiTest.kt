package io.swagger.client.apis

import io.swagger.client.infrastructure.ApiClient
import io.swagger.client.models.Client
import org.junit.Assert
import org.junit.Before
import org.junit.Test

/**
 * API tests for AnotherFakeApi
 */
class AnotherFakeApiTest {

    private lateinit var api: AnotherFakeApi

    @Before
    fun setup() {
        api = ApiClient().createService(AnotherFakeApi::class.java)
    }

    /**
     * To test special tags
     *
     * To test special tags
     */
    @Test
    fun testSpecialTagsTest() {
        val body: Client? = null
        // val response = api.testSpecialTags(body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }
}
