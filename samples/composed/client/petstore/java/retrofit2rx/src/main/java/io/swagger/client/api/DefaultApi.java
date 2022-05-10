package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import rx.Observable;
import retrofit2.http.*;

import okhttp3.RequestBody;
import okhttp3.ResponseBody;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface DefaultApi {
  /**
   * 
   * 
   * @return Call&lt;List&lt;String&gt;&gt;
   */
  @GET("test")
  Observable<List<String>> testMethod();
    

}
