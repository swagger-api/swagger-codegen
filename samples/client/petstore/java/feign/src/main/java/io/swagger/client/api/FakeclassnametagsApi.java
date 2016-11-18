package io.swagger.client.api;

import feign.*;
import io.swagger.client.ApiClient;
import io.swagger.client.model.Client;


public interface FakeclassnametagsApi extends ApiClient.Api {


  /**
   * To test class name in snake case
   * 
   * @param body client model (required)
   * @return Client
   */
  @RequestLine("PATCH /fake_classname_test")
  @Headers({
    "Content-type: application/json",
    "Accept: application/json",
  })
  Client testClassname(Client body);
}
