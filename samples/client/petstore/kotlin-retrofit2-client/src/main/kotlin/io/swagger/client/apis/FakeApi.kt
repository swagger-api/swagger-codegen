package io.swagger.client.apis

import retrofit2.Call
import retrofit2.http.*

import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody

import io.swagger.client.models.Client
import io.swagger.client.models.OuterBoolean
import io.swagger.client.models.OuterComposite
import io.swagger.client.models.OuterNumber
import io.swagger.client.models.OuterString
import io.swagger.client.models.User

interface FakeApi {
  /**
   * 
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return Call&lt;OuterBoolean&gt;
   */
  @POST("fake/outer/boolean")
  fun fakeOuterBooleanSerialize(
    @retrofit2.http.Body body: OuterBoolean
  ): Call<OuterBoolean>

  /**
   * 
   * Test serialization of object with outer number type
   * @param body Input composite as post body (optional)
   * @return Call&lt;OuterComposite&gt;
   */
  @POST("fake/outer/composite")
  fun fakeOuterCompositeSerialize(
    @retrofit2.http.Body body: OuterComposite
  ): Call<OuterComposite>

  /**
   * 
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return Call&lt;OuterNumber&gt;
   */
  @POST("fake/outer/number")
  fun fakeOuterNumberSerialize(
    @retrofit2.http.Body body: OuterNumber
  ): Call<OuterNumber>

  /**
   * 
   * Test serialization of outer string types
   * @param body Input string as post body (optional)
   * @return Call&lt;OuterString&gt;
   */
  @POST("fake/outer/string")
  fun fakeOuterStringSerialize(
    @retrofit2.http.Body body: OuterString
  ): Call<OuterString>

  /**
   * 
   * 
   * @param body  (required)
   * @param query  (required)
   * @return Call&lt;Void&gt;
   */
  @Headers(
    "Content-Type:application/json"
  )
  @PUT("fake/body-with-query-params")
  fun testBodyWithQueryParams(
    @retrofit2.http.Body body: User,
    @retrofit2.http.Query("query") query: kotlin.String
  ): Call<Void>

  /**
   * To test \&quot;client\&quot; model
   * To test \&quot;client\&quot; model
   * @param body client model (required)
   * @return Call&lt;Client&gt;
   */
  @Headers(
    "Content-Type:application/json"
  )
  @PATCH("fake")
  fun testClientModel(
    @retrofit2.http.Body body: Client
  ): Call<Client>

  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param number None (required)
   * @param double None (required)
   * @param patternWithoutDelimiter None (required)
   * @param byte None (required)
   * @param integer None (optional)
   * @param int32 None (optional)
   * @param int64 None (optional)
   * @param float None (optional)
   * @param string None (optional)
   * @param binary None (optional)
   * @param date None (optional)
   * @param dateTime None (optional)
   * @param password None (optional)
   * @param callback None (optional)
   * @return Call&lt;Void&gt;
   */
  @retrofit2.http.FormUrlEncoded
  @POST("fake")
  fun testEndpointParameters(
    @retrofit2.http.Field("number") number: java.math.BigDecimal,
    @retrofit2.http.Field("double") double: kotlin.Double,
    @retrofit2.http.Field("pattern_without_delimiter") patternWithoutDelimiter: kotlin.String,
    @retrofit2.http.Field("byte") byte: kotlin.ByteArray,
    @retrofit2.http.Field("integer") integer: kotlin.Int? = null,
    @retrofit2.http.Field("int32") int32: kotlin.Int? = null,
    @retrofit2.http.Field("int64") int64: kotlin.Long? = null,
    @retrofit2.http.Field("float") float: kotlin.Float? = null,
    @retrofit2.http.Field("string") string: kotlin.String? = null,
    @retrofit2.http.Field("binary") binary: kotlin.Array<kotlin.Byte>? = null,
    @retrofit2.http.Field("date") date: java.time.OffsetDateTime? = null,
    @retrofit2.http.Field("dateTime") dateTime: java.time.OffsetDateTime? = null,
    @retrofit2.http.Field("password") password: kotlin.String? = null,
    @retrofit2.http.Field("callback") callback: kotlin.String? = null
  ): Call<Void>

  /**
   * To test enum parameters
   * To test enum parameters
   * @param enumFormStringArray Form parameter enum test (string array) (optional)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @return Call&lt;Void&gt;
   */
  @retrofit2.http.FormUrlEncoded
  @GET("fake")
  fun testEnumParameters(
    @retrofit2.http.Field("enum_form_string_array") enumFormStringArray: kotlin.collections.List<kotlin.String>? = null,
    @retrofit2.http.Field("enum_form_string") enumFormString: kotlin.String? = null,
    @retrofit2.http.Header("enum_header_string_array") enumHeaderStringArray: kotlin.collections.List<kotlin.String>? = null,
    @retrofit2.http.Header("enum_header_string") enumHeaderString: kotlin.String? = null,
    @retrofit2.http.Query("enum_query_string_array") enumQueryStringArray: kotlin.collections.List<kotlin.String>? = null,
    @retrofit2.http.Query("enum_query_string") enumQueryString: kotlin.String? = null,
    @retrofit2.http.Query("enum_query_integer") enumQueryInteger: kotlin.Int? = null,
    @retrofit2.http.Field("enum_query_double") enumQueryDouble: kotlin.Double? = null
  ): Call<Void>

  /**
   * test inline additionalProperties
   * 
   * @param param request body (required)
   * @return Call&lt;Void&gt;
   */
  @Headers(
    "Content-Type:application/json"
  )
  @POST("fake/inline-additionalProperties")
  fun testInlineAdditionalProperties(
    @retrofit2.http.Body param: kotlin.Any
  ): Call<Void>

  /**
   * test json serialization of form data
   * 
   * @param param field1 (required)
   * @param param2 field2 (required)
   * @return Call&lt;Void&gt;
   */
  @retrofit2.http.FormUrlEncoded
  @GET("fake/jsonFormData")
  fun testJsonFormData(
    @retrofit2.http.Field("param") param: kotlin.String,
    @retrofit2.http.Field("param2") param2: kotlin.String
  ): Call<Void>

}
