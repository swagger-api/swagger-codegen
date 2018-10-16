package io.swagger.client.api;

import io.swagger.client.ApiClient;
import io.swagger.client.EncodingUtils;

import io.swagger.client.model.Client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;


public interface FakeClassnameTags123Api extends ApiClient.Api {


  /**
   * To test class name in snake case
   * To test class name in snake case
    * @param body client model (required)
   * @return Client
   */
  @RequestLine("PATCH /fake_classname_test?")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  Client testClassname(Client body);

  /**
   * To test class name in snake case
   * To test class name in snake case
   * Note, this is equivalent to the other <code>testClassname</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestClassnameQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param body client model (required)
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   </ul>
   * @return Client
   */
  @RequestLine("PATCH /fake_classname_test?")
  @Headers({
  "Content-Type: application/json",
  "Accept: application/json",
  })
  Client testClassname(Client body, @QueryMap(encoded=true) Map<String, Object> queryParams);

  /**
   * A convenience class for generating query parameters for the
   * <code>testClassname</code> method in a fluent style.
   */
  public static class TestClassnameQueryParams extends HashMap<String, Object> {
  }
}
