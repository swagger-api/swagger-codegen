package io.swagger.client.apis

import io.swagger.client.infrastructure.ApiClient
import io.swagger.client.models.Client
import org.junit.Assert
import org.junit.Before
import org.junit.Test

/**
 * API tests for FakeClassnameTags123Api
 */
class FakeClassnameTags123ApiTest {

    private lateinit var api: FakeClassnameTags123Api

    @Before
    fun setup() {
        api = ApiClient().createService(FakeClassnameTags123Api::class.java)
    }

    /**
     * To test class name in snake case
     *
     * To test class name in snake case
     */
    @Test
    fun testClassnameTest() {
        val body: Client? = null
        // val response = api.testClassname(body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }
}
