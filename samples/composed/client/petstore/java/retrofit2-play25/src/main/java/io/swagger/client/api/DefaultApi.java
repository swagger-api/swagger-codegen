package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;



import retrofit2.Call;
import retrofit2.http.*;

import okhttp3.RequestBody;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.concurrent.*;
import retrofit2.Response;

public interface DefaultApi {
  /**
   * 
   * 
   * @return Call&lt;List&lt;String&gt;&gt;
   */
  @GET("test")
  CompletionStage<Response<List<String>>> testMethod();
    

}
