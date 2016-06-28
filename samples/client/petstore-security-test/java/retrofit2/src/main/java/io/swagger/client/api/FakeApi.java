package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;


import retrofit2.Call;
import retrofit2.http.*;

import okhttp3.RequestBody;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface FakeApi {
  /**
   * To test code injection  &#39; \&quot; &#x3D;end
   * 
   * @param testCodeInjectEnd To test code injection  &#39; \&quot; &#x3D;end (optional)
   * @return Call<Void>
   */
  
  @FormUrlEncoded
  @PUT("fake")
  Call<Void> testCodeInjectEnd(
    @Field("test code inject */ &#39; &quot; &#x3D;end") String testCodeInjectEnd
  );

}
