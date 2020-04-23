package io.swagger.client.api;

import io.swagger.client.ApiClient;
import io.swagger.client.EncodingUtils;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

public interface DefaultApi extends ApiClient.Api {

  /**
   * 
   * 
   * @return List&lt;String&gt;
   */
  @RequestLine("GET /test")
  @Headers({
      "Accept: application/json",
  })
  List<String> testMethod();
}
