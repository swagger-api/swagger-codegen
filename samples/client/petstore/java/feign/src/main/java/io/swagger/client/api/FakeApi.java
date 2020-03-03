package io.swagger.client.api;

import io.swagger.client.ApiClient;
import io.swagger.client.EncodingUtils;

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
import feign.*;

public interface FakeApi extends ApiClient.Api {

  /**
   * 
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return Boolean
   */
  @RequestLine("POST /fake/outer/boolean")
  @Headers({
      "Content-Type: */*",
      "Accept: */*",
  })
  Boolean fakeOuterBooleanSerialize(Boolean body);
  /**
   * 
   * Test serialization of object with outer number type
   * @param body Input composite as post body (optional)
   * @return OuterComposite
   */
  @RequestLine("POST /fake/outer/composite")
  @Headers({
      "Content-Type: */*",
      "Accept: */*",
  })
  OuterComposite fakeOuterCompositeSerialize(OuterComposite body);
  /**
   * 
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return BigDecimal
   */
  @RequestLine("POST /fake/outer/number")
  @Headers({
      "Content-Type: */*",
      "Accept: */*",
  })
  BigDecimal fakeOuterNumberSerialize(BigDecimal body);
  /**
   * 
   * Test serialization of outer string types
   * @param body Input string as post body (optional)
   * @return String
   */
  @RequestLine("POST /fake/outer/string")
  @Headers({
      "Content-Type: */*",
      "Accept: */*",
  })
  String fakeOuterStringSerialize(String body);
  /**
   * To test \&quot;client\&quot; model
   * To test \&quot;client\&quot; model
   * @param body client model (required)
   * @return Client
   */
  @RequestLine("PATCH /fake")
  @Headers({
      "Content-Type: application/json",
      "Accept: application/json",
  })
  Client testClientModel(Client body);
  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param body  (required)
   */
  @RequestLine("POST /fake")
  @Headers({
      "Content-Type: application/xml; charset&#x3D;utf-8",
      "Accept: */*",
  })
  void testEndpointParameters(Body2 body);
  /**
   * To test enum parameters
   * To test enum parameters
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   */
  @RequestLine("GET /fake?enum_query_string_array={enumQueryStringArray}&enum_query_string={enumQueryString}&enum_query_integer={enumQueryInteger}")
  @Headers({
      "Accept: */*",
    "enum_header_string_array: {enumHeaderStringArray}",
    "enum_header_string: {enumHeaderString}"
  })
  void testEnumParameters(@Param("enumHeaderStringArray") List<String> enumHeaderStringArray, @Param("enumHeaderString") String enumHeaderString, @Param("enumQueryStringArray") List<String> enumQueryStringArray, @Param("enumQueryString") String enumQueryString, @Param("enumQueryInteger") Integer enumQueryInteger);

  /**
   * To test enum parameters
   * To test enum parameters
   * Note, this is equivalent to the other <code>testEnumParameters</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestEnumParametersQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>enumQueryStringArray - Query parameter enum test (string array) (optional)</li>
   *   <li>enumQueryString - Query parameter enum test (string) (optional, default to -efg)</li>
   *   <li>enumQueryInteger - Query parameter enum test (double) (optional)</li>
   *   </ul>

   */
  @RequestLine("GET /fake?enum_query_string_array={enumQueryStringArray}&enum_query_string={enumQueryString}&enum_query_integer={enumQueryInteger}")
  @Headers({
      "Content-Type: */*",
      "enum_header_string_array: {enumHeaderStringArray}",
      "enum_header_string: {enumHeaderString}"
  })
  void testEnumParameters(@Param("enumHeaderStringArray") List<String> enumHeaderStringArray, @Param("enumHeaderString") String enumHeaderString, @QueryMap(encoded=true) Map<String, Object> queryParams);

  /**
   * A convenience class for generating query parameters for the
   * <code>testEnumParameters</code> method in a fluent style.
   */
  public static class TestEnumParametersQueryParams extends HashMap<String, Object> {
    public TestEnumParametersQueryParams enumQueryStringArray(final List<String> value) {
      put("enum_query_string_array", EncodingUtils.encodeCollection(value, "csv"));
      return this;
    }
    public TestEnumParametersQueryParams enumQueryString(final String value) {
      put("enum_query_string", EncodingUtils.encode(value));
      return this;
    }
    public TestEnumParametersQueryParams enumQueryInteger(final Integer value) {
      put("enum_query_integer", EncodingUtils.encode(value));
      return this;
    }
  }
  /**
   * To test enum parameters
   * To test enum parameters
   * @param body  (optional)
   */
  @RequestLine("POST /fake/enum/form")
  @Headers({
      "Content-Type: */*",
      "Accept: */*",
  })
  void testEnumRequestBody(Body4 body);
  /**
   * test inline additionalProperties
   * 
   * @param body request body (required)
   */
  @RequestLine("POST /fake/inline-additionalProperties")
  @Headers({
      "Content-Type: application/json",
      "Accept: */*",
  })
  void testInlineAdditionalProperties(Map<String, String> body);
  /**
   * test json serialization of form data
   * 
   * @param body  (required)
   */
  @RequestLine("POST /fake/jsonFormData")
  @Headers({
      "Content-Type: application/json",
      "Accept: */*",
  })
  void testJsonFormData(Body5 body);
}
