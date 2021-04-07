package io.swagger.client.apis

import io.swagger.client.infrastructure.ApiClient
import io.swagger.client.models.User
import org.junit.Assert
import org.junit.Before
import org.junit.Test

/**
 * API tests for UserApi
 */
class UserApiTest {

    private lateinit var api: UserApi

    @Before
    fun setup() {
        api = ApiClient().createService(UserApi::class.java)
    }

    /**
     * Create user
     *
     * This can only be done by the logged in user.
     */
    @Test
    fun createUserTest() {
        val body: User? = null
        // val response = api.createUser(body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Creates list of users with given input array
     *
     * 
     */
    @Test
    fun createUsersWithArrayInputTest() {
        val body: kotlin.collections.List<User>? = null
        // val response = api.createUsersWithArrayInput(body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Creates list of users with given input array
     *
     * 
     */
    @Test
    fun createUsersWithListInputTest() {
        val body: kotlin.collections.List<User>? = null
        // val response = api.createUsersWithListInput(body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Delete user
     *
     * This can only be done by the logged in user.
     */
    @Test
    fun deleteUserTest() {
        val username: kotlin.String? = null
        // val response = api.deleteUser(username).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Get user by user name
     *
     * 
     */
    @Test
    fun getUserByNameTest() {
        val username: kotlin.String? = null
        // val response = api.getUserByName(username).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Logs user into the system
     *
     * 
     */
    @Test
    fun loginUserTest() {
        val username: kotlin.String? = null
        val password: kotlin.String? = null
        // val response = api.loginUser(username, password).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Logs out current logged in user session
     *
     * 
     */
    @Test
    fun logoutUserTest() {
        // val response = api.logoutUser().execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Updated user
     *
     * This can only be done by the logged in user.
     */
    @Test
    fun updateUserTest() {
        val username: kotlin.String? = null
        val body: User? = null
        // val response = api.updateUser(username, body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }
}
