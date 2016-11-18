package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;
import io.swagger.client.model.Client;
import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

public interface FakeclassnametagsApi {
  /**
   * To test class name in snake case
   * Sync method
   * 
   * @param body client model (required)
   * @return Client
   */
  
  @PATCH("/fake_classname_test")
  Client testClassname(
    @retrofit.http.Body Client body
  );

  /**
   * To test class name in snake case
   * Async method
   * @param body client model (required)
   * @param cb callback method
   * @return void
   */
  
  @PATCH("/fake_classname_test")
  void testClassname(
    @retrofit.http.Body Client body, Callback<Client> cb
  );
}
