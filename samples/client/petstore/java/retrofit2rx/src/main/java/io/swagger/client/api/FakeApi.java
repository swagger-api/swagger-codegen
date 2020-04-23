package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import rx.Observable;
import retrofit2.http.*;

import okhttp3.RequestBody;
import okhttp3.ResponseBody;

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
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return Call&lt;Boolean&gt;
   */
  @Headers({
    "Content-Type:*/*"
  })
  @POST("fake/outer/boolean")
  Observable<Boolean> fakeOuterBooleanSerialize(
                    @retrofit2.http.Body Boolean body    
  );

  /**
   * 
   * Test serialization of object with outer number type
   * @param body Input composite as post body (optional)
   * @return Call&lt;OuterComposite&gt;
   */
  @Headers({
    "Content-Type:*/*"
  })
  @POST("fake/outer/composite")
  Observable<OuterComposite> fakeOuterCompositeSerialize(
                    @retrofit2.http.Body OuterComposite body    
  );

  /**
   * 
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return Call&lt;BigDecimal&gt;
   */
  @Headers({
    "Content-Type:*/*"
  })
  @POST("fake/outer/number")
  Observable<BigDecimal> fakeOuterNumberSerialize(
                    @retrofit2.http.Body BigDecimal body    
  );

  /**
   * 
   * Test serialization of outer string types
   * @param body Input string as post body (optional)
   * @return Call&lt;String&gt;
   */
  @Headers({
    "Content-Type:*/*"
  })
  @POST("fake/outer/string")
  Observable<String> fakeOuterStringSerialize(
                    @retrofit2.http.Body String body    
  );

  /**
   * To test \&quot;client\&quot; model
   * To test \&quot;client\&quot; model
   * @param body client model (required)
   * @return Call&lt;Client&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PATCH("fake")
  Observable<Client> testClientModel(
                    @retrofit2.http.Body Client body    
  );

  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param body  (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json; charset&#x3D;utf-8"
  })
  @POST("fake")
  Observable<Void> testEndpointParameters(
                    @retrofit2.http.Body Body2 body    
  );

  /**
   * To test enum parameters
   * To test enum parameters
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @return Call&lt;Void&gt;
   */
  @GET("fake")
  Observable<Void> testEnumParameters(
                @retrofit2.http.Header("enum_header_string_array") List<String> enumHeaderStringArray        ,             @retrofit2.http.Header("enum_header_string") String enumHeaderString        ,     @retrofit2.http.Query("enum_query_string_array") List<String> enumQueryStringArray                ,     @retrofit2.http.Query("enum_query_string") String enumQueryString                ,     @retrofit2.http.Query("enum_query_integer") Integer enumQueryInteger                
  );

  /**
   * To test enum parameters
   * To test enum parameters
   * @param body  (optional)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:*/*"
  })
  @POST("fake/enum/form")
  Observable<Void> testEnumRequestBody(
                    @retrofit2.http.Body Body4 body    
  );

  /**
   * test inline additionalProperties
   * 
   * @param body request body (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @POST("fake/inline-additionalProperties")
  Observable<Void> testInlineAdditionalProperties(
                    @retrofit2.http.Body Map<String, String> body    
  );

  /**
   * test json serialization of form data
   * 
   * @param body  (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @POST("fake/jsonFormData")
  Observable<Void> testJsonFormData(
                    @retrofit2.http.Body Body5 body    
  );

}
