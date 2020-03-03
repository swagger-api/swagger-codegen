package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import java.math.BigDecimal;
import io.swagger.client.model.Body2;
import io.swagger.client.model.Body3;
import io.swagger.client.model.Body4;
import io.swagger.client.model.Body5;
import io.swagger.client.model.Client;
import io.swagger.client.model.OuterComposite;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface FakeApi {
  /**
   * 
   * Sync method
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return Boolean
   */
  @POST("/fake/outer/boolean")
  Boolean fakeOuterBooleanSerialize(
    @retrofit.http.Body Boolean body
  );

  /**
   * 
   * Async method
   * @param body Input boolean as post body (optional)
   * @param cb callback method
   */
  @POST("/fake/outer/boolean")
  void fakeOuterBooleanSerialize(
    @retrofit.http.Body Boolean body, Callback<Boolean> cb
  );
  /**
   * 
   * Sync method
   * Test serialization of object with outer number type
   * @param body Input composite as post body (optional)
   * @return OuterComposite
   */
  @POST("/fake/outer/composite")
  OuterComposite fakeOuterCompositeSerialize(
    @retrofit.http.Body OuterComposite body
  );

  /**
   * 
   * Async method
   * @param body Input composite as post body (optional)
   * @param cb callback method
   */
  @POST("/fake/outer/composite")
  void fakeOuterCompositeSerialize(
    @retrofit.http.Body OuterComposite body, Callback<OuterComposite> cb
  );
  /**
   * 
   * Sync method
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return BigDecimal
   */
  @POST("/fake/outer/number")
  BigDecimal fakeOuterNumberSerialize(
    @retrofit.http.Body BigDecimal body
  );

  /**
   * 
   * Async method
   * @param body Input number as post body (optional)
   * @param cb callback method
   */
  @POST("/fake/outer/number")
  void fakeOuterNumberSerialize(
    @retrofit.http.Body BigDecimal body, Callback<BigDecimal> cb
  );
  /**
   * 
   * Sync method
   * Test serialization of outer string types
   * @param body Input string as post body (optional)
   * @return String
   */
  @POST("/fake/outer/string")
  String fakeOuterStringSerialize(
    @retrofit.http.Body String body
  );

  /**
   * 
   * Async method
   * @param body Input string as post body (optional)
   * @param cb callback method
   */
  @POST("/fake/outer/string")
  void fakeOuterStringSerialize(
    @retrofit.http.Body String body, Callback<String> cb
  );
  /**
   * To test \&quot;client\&quot; model
   * Sync method
   * To test \&quot;client\&quot; model
   * @param body client model (required)
   * @return Client
   */
  @PATCH("/fake")
  Client testClientModel(
    @retrofit.http.Body Client body
  );

  /**
   * To test \&quot;client\&quot; model
   * Async method
   * @param body client model (required)
   * @param cb callback method
   */
  @PATCH("/fake")
  void testClientModel(
    @retrofit.http.Body Client body, Callback<Client> cb
  );
  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Sync method
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param body  (required)
   * @return Void
   */
  @POST("/fake")
  Void testEndpointParameters(
    @retrofit.http.Body Body2 body
  );

  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Async method
   * @param body  (required)
   * @param cb callback method
   */
  @POST("/fake")
  void testEndpointParameters(
    @retrofit.http.Body Body2 body, Callback<Void> cb
  );
  /**
   * To test enum parameters
   * Sync method
   * To test enum parameters
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @return Void
   */
  @GET("/fake")
  Void testEnumParameters(
    @retrofit.http.Header("enum_header_string_array") List<String> enumHeaderStringArray, @retrofit.http.Header("enum_header_string") String enumHeaderString, @retrofit.http.Query("enum_query_string_array") List<String> enumQueryStringArray, @retrofit.http.Query("enum_query_string") String enumQueryString, @retrofit.http.Query("enum_query_integer") Integer enumQueryInteger
  );

  /**
   * To test enum parameters
   * Async method
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param cb callback method
   */
  @GET("/fake")
  void testEnumParameters(
    @retrofit.http.Header("enum_header_string_array") List<String> enumHeaderStringArray, @retrofit.http.Header("enum_header_string") String enumHeaderString, @retrofit.http.Query("enum_query_string_array") List<String> enumQueryStringArray, @retrofit.http.Query("enum_query_string") String enumQueryString, @retrofit.http.Query("enum_query_integer") Integer enumQueryInteger, Callback<Void> cb
  );
  /**
   * To test enum parameters
   * Sync method
   * To test enum parameters
   * @param body  (optional)
   * @return Void
   */
  @POST("/fake/enum/form")
  Void testEnumRequestBody(
    @retrofit.http.Body Body4 body
  );

  /**
   * To test enum parameters
   * Async method
   * @param body  (optional)
   * @param cb callback method
   */
  @POST("/fake/enum/form")
  void testEnumRequestBody(
    @retrofit.http.Body Body4 body, Callback<Void> cb
  );
  /**
   * test inline additionalProperties
   * Sync method
   * 
   * @param body request body (required)
   * @return Void
   */
  @POST("/fake/inline-additionalProperties")
  Void testInlineAdditionalProperties(
    @retrofit.http.Body Map<String, String> body
  );

  /**
   * test inline additionalProperties
   * Async method
   * @param body request body (required)
   * @param cb callback method
   */
  @POST("/fake/inline-additionalProperties")
  void testInlineAdditionalProperties(
    @retrofit.http.Body Map<String, String> body, Callback<Void> cb
  );
  /**
   * test json serialization of form data
   * Sync method
   * 
   * @param body  (required)
   * @return Void
   */
  @POST("/fake/jsonFormData")
  Void testJsonFormData(
    @retrofit.http.Body Body5 body
  );

  /**
   * test json serialization of form data
   * Async method
   * @param body  (required)
   * @param cb callback method
   */
  @POST("/fake/jsonFormData")
  void testJsonFormData(
    @retrofit.http.Body Body5 body, Callback<Void> cb
  );
}
