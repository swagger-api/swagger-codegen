package io.swagger.client.apis

import retrofit2.Call
import retrofit2.http.*

import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody

import io.swagger.client.models.Client

interface FakeClassnameTags123Api {
  /**
   * To test class name in snake case
   * To test class name in snake case
   * @param body client model (required)
   * @return Call&lt;Client&gt;
   */
  @Headers(
    "Content-Type:application/json"
  )
  @PATCH("fake_classname_test")
  fun testClassname(
    @retrofit2.http.Body body: Client
  ): Call<Client>

}
