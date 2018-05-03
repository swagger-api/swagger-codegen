package io.swagger.client.apis

import retrofit2.Call
import retrofit2.http.*

import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody

import io.swagger.client.models.User

interface UserApi {
  /**
   * Create user
   * This can only be done by the logged in user.
   * @param body Created user object (required)
   * @return Call&lt;Void&gt;
   */
  @POST("user")
  fun createUser(
    @retrofit2.http.Body body: User
  ): Call<Void>

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object (required)
   * @return Call&lt;Void&gt;
   */
  @POST("user/createWithArray")
  fun createUsersWithArrayInput(
    @retrofit2.http.Body body: kotlin.collections.List<User>
  ): Call<Void>

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object (required)
   * @return Call&lt;Void&gt;
   */
  @POST("user/createWithList")
  fun createUsersWithListInput(
    @retrofit2.http.Body body: kotlin.collections.List<User>
  ): Call<Void>

  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted (required)
   * @return Call&lt;Void&gt;
   */
  @DELETE("user/{username}")
  fun deleteUser(
    @retrofit2.http.Path("username") username: kotlin.String
  ): Call<Void>

  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing. (required)
   * @return Call&lt;User&gt;
   */
  @GET("user/{username}")
  fun getUserByName(
    @retrofit2.http.Path("username") username: kotlin.String
  ): Call<User>

  /**
   * Logs user into the system
   * 
   * @param username The user name for login (required)
   * @param password The password for login in clear text (required)
   * @return Call&lt;kotlin.String&gt;
   */
  @GET("user/login")
  fun loginUser(
    @retrofit2.http.Query("username") username: kotlin.String,
    @retrofit2.http.Query("password") password: kotlin.String
  ): Call<kotlin.String>

  /**
   * Logs out current logged in user session
   * 
   * @return Call&lt;Void&gt;
   */
  @GET("user/logout")
  fun logoutUser()
    : Call<Void>

  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted (required)
   * @param body Updated user object (required)
   * @return Call&lt;Void&gt;
   */
  @PUT("user/{username}")
  fun updateUser(
    @retrofit2.http.Path("username") username: kotlin.String,
    @retrofit2.http.Body body: User
  ): Call<Void>

}
