package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface DefaultApi {
  /**
   * 
   * Sync method
   * 
   * @return List&lt;String&gt;
   */
  @GET("/test")
  List<String> testMethod();
    

  /**
   * 
   * Async method
   * @param cb callback method
   */
  @GET("/test")
  void testMethod(
    Callback<List<String>> cb
  );
}
