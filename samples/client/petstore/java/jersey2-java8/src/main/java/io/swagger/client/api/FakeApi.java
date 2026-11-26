package io.swagger.client.api;

import io.swagger.client.ApiException;
import io.swagger.client.ApiClient;
import io.swagger.client.ApiResponse;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;

import javax.ws.rs.core.GenericType;

import java.math.BigDecimal;
import io.swagger.client.model.Client;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import io.swagger.client.model.OuterComposite;
import io.swagger.client.model.User;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class FakeApi {
  private ApiClient apiClient;

  public FakeApi() {
    this(Configuration.getDefaultApiClient());
  }

  public FakeApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public static class FakeOuterBooleanSerializeOptionals {
    
    public Boolean body() {
      return this.body;
    }

    public FakeOuterBooleanSerializeOptionals body(Boolean body) {
      this.body = body;
      return this;
    }

    private Boolean body = null;
    
  }

  /**
   * 
   * Test serialization of outer boolean types
   * @return Boolean
   * @throws ApiException if fails to make API call
   */
  public Boolean fakeOuterBooleanSerialize() throws ApiException {
    return fakeOuterBooleanSerialize(null);
  }

  /**
   * 
   * Test serialization of outer boolean types
   * @param optionals An object containing the optional parameters for this API call.
   * @return Boolean
   * @throws ApiException if fails to make API call
   */
  public Boolean fakeOuterBooleanSerializeOpts(FakeOuterBooleanSerializeOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new FakeOuterBooleanSerializeOptionals();
    }
    return fakeOuterBooleanSerialize(optionals.body());
  }

  /**
   * 
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return Boolean
   * @throws ApiException if fails to make API call
   */
  public Boolean fakeOuterBooleanSerialize(Boolean body) throws ApiException {
    return fakeOuterBooleanSerializeWithHttpInfo(body).getData();
      }

  /**
   * 
   * Test serialization of outer boolean types
   * @return ApiResponse&lt;Boolean&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Boolean> fakeOuterBooleanSerializeWithHttpInfo() throws ApiException {
    return fakeOuterBooleanSerializeWithHttpInfo(null);
  }

  /**
   * 
   * Test serialization of outer boolean types
   * @param optionals An object containing the optional parameters for this API call.
   * @return ApiResponse&lt;Boolean&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Boolean> fakeOuterBooleanSerializeOptsWithHttpInfo(FakeOuterBooleanSerializeOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new FakeOuterBooleanSerializeOptionals();
    }
    return fakeOuterBooleanSerializeWithHttpInfo(optionals.body());
  }

  /**
   * 
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return ApiResponse&lt;Boolean&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Boolean> fakeOuterBooleanSerializeWithHttpInfo(Boolean body) throws ApiException {
    Object localVarPostBody = body;
    
    // create path and map variables
    String localVarPath = "/fake/outer/boolean";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<Boolean> localVarReturnType = new GenericType<Boolean>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  public static class FakeOuterCompositeSerializeOptionals {
    
    public OuterComposite body() {
      return this.body;
    }

    public FakeOuterCompositeSerializeOptionals body(OuterComposite body) {
      this.body = body;
      return this;
    }

    private OuterComposite body = null;
    
  }

  /**
   * 
   * Test serialization of object with outer number type
   * @return OuterComposite
   * @throws ApiException if fails to make API call
   */
  public OuterComposite fakeOuterCompositeSerialize() throws ApiException {
    return fakeOuterCompositeSerialize(null);
  }

  /**
   * 
   * Test serialization of object with outer number type
   * @param optionals An object containing the optional parameters for this API call.
   * @return OuterComposite
   * @throws ApiException if fails to make API call
   */
  public OuterComposite fakeOuterCompositeSerializeOpts(FakeOuterCompositeSerializeOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new FakeOuterCompositeSerializeOptionals();
    }
    return fakeOuterCompositeSerialize(optionals.body());
  }

  /**
   * 
   * Test serialization of object with outer number type
   * @param body Input composite as post body (optional)
   * @return OuterComposite
   * @throws ApiException if fails to make API call
   */
  public OuterComposite fakeOuterCompositeSerialize(OuterComposite body) throws ApiException {
    return fakeOuterCompositeSerializeWithHttpInfo(body).getData();
      }

  /**
   * 
   * Test serialization of object with outer number type
   * @return ApiResponse&lt;OuterComposite&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<OuterComposite> fakeOuterCompositeSerializeWithHttpInfo() throws ApiException {
    return fakeOuterCompositeSerializeWithHttpInfo(null);
  }

  /**
   * 
   * Test serialization of object with outer number type
   * @param optionals An object containing the optional parameters for this API call.
   * @return ApiResponse&lt;OuterComposite&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<OuterComposite> fakeOuterCompositeSerializeOptsWithHttpInfo(FakeOuterCompositeSerializeOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new FakeOuterCompositeSerializeOptionals();
    }
    return fakeOuterCompositeSerializeWithHttpInfo(optionals.body());
  }

  /**
   * 
   * Test serialization of object with outer number type
   * @param body Input composite as post body (optional)
   * @return ApiResponse&lt;OuterComposite&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<OuterComposite> fakeOuterCompositeSerializeWithHttpInfo(OuterComposite body) throws ApiException {
    Object localVarPostBody = body;
    
    // create path and map variables
    String localVarPath = "/fake/outer/composite";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<OuterComposite> localVarReturnType = new GenericType<OuterComposite>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  public static class FakeOuterNumberSerializeOptionals {
    
    public BigDecimal body() {
      return this.body;
    }

    public FakeOuterNumberSerializeOptionals body(BigDecimal body) {
      this.body = body;
      return this;
    }

    private BigDecimal body = null;
    
  }

  /**
   * 
   * Test serialization of outer number types
   * @return BigDecimal
   * @throws ApiException if fails to make API call
   */
  public BigDecimal fakeOuterNumberSerialize() throws ApiException {
    return fakeOuterNumberSerialize(null);
  }

  /**
   * 
   * Test serialization of outer number types
   * @param optionals An object containing the optional parameters for this API call.
   * @return BigDecimal
   * @throws ApiException if fails to make API call
   */
  public BigDecimal fakeOuterNumberSerializeOpts(FakeOuterNumberSerializeOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new FakeOuterNumberSerializeOptionals();
    }
    return fakeOuterNumberSerialize(optionals.body());
  }

  /**
   * 
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return BigDecimal
   * @throws ApiException if fails to make API call
   */
  public BigDecimal fakeOuterNumberSerialize(BigDecimal body) throws ApiException {
    return fakeOuterNumberSerializeWithHttpInfo(body).getData();
      }

  /**
   * 
   * Test serialization of outer number types
   * @return ApiResponse&lt;BigDecimal&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<BigDecimal> fakeOuterNumberSerializeWithHttpInfo() throws ApiException {
    return fakeOuterNumberSerializeWithHttpInfo(null);
  }

  /**
   * 
   * Test serialization of outer number types
   * @param optionals An object containing the optional parameters for this API call.
   * @return ApiResponse&lt;BigDecimal&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<BigDecimal> fakeOuterNumberSerializeOptsWithHttpInfo(FakeOuterNumberSerializeOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new FakeOuterNumberSerializeOptionals();
    }
    return fakeOuterNumberSerializeWithHttpInfo(optionals.body());
  }

  /**
   * 
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return ApiResponse&lt;BigDecimal&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<BigDecimal> fakeOuterNumberSerializeWithHttpInfo(BigDecimal body) throws ApiException {
    Object localVarPostBody = body;
    
    // create path and map variables
    String localVarPath = "/fake/outer/number";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<BigDecimal> localVarReturnType = new GenericType<BigDecimal>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  public static class FakeOuterStringSerializeOptionals {
    
    public String body() {
      return this.body;
    }

    public FakeOuterStringSerializeOptionals body(String body) {
      this.body = body;
      return this;
    }

    private String body = null;
    
  }

  /**
   * 
   * Test serialization of outer string types
   * @return String
   * @throws ApiException if fails to make API call
   */
  public String fakeOuterStringSerialize() throws ApiException {
    return fakeOuterStringSerialize(null);
  }

  /**
   * 
   * Test serialization of outer string types
   * @param optionals An object containing the optional parameters for this API call.
   * @return String
   * @throws ApiException if fails to make API call
   */
  public String fakeOuterStringSerializeOpts(FakeOuterStringSerializeOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new FakeOuterStringSerializeOptionals();
    }
    return fakeOuterStringSerialize(optionals.body());
  }

  /**
   * 
   * Test serialization of outer string types
   * @param body Input string as post body (optional)
   * @return String
   * @throws ApiException if fails to make API call
   */
  public String fakeOuterStringSerialize(String body) throws ApiException {
    return fakeOuterStringSerializeWithHttpInfo(body).getData();
      }

  /**
   * 
   * Test serialization of outer string types
   * @return ApiResponse&lt;String&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<String> fakeOuterStringSerializeWithHttpInfo() throws ApiException {
    return fakeOuterStringSerializeWithHttpInfo(null);
  }

  /**
   * 
   * Test serialization of outer string types
   * @param optionals An object containing the optional parameters for this API call.
   * @return ApiResponse&lt;String&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<String> fakeOuterStringSerializeOptsWithHttpInfo(FakeOuterStringSerializeOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new FakeOuterStringSerializeOptionals();
    }
    return fakeOuterStringSerializeWithHttpInfo(optionals.body());
  }

  /**
   * 
   * Test serialization of outer string types
   * @param body Input string as post body (optional)
   * @return ApiResponse&lt;String&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<String> fakeOuterStringSerializeWithHttpInfo(String body) throws ApiException {
    Object localVarPostBody = body;
    
    // create path and map variables
    String localVarPath = "/fake/outer/string";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }


  /**
   * 
   * 
   * @param body  (required)
   * @param query  (required)
   * @throws ApiException if fails to make API call
   */
  public void testBodyWithQueryParams(User body, String query) throws ApiException {

    testBodyWithQueryParamsWithHttpInfo(body, query);
  }


  /**
   * 
   * 
   * @param body  (required)
   * @param query  (required)
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testBodyWithQueryParamsWithHttpInfo(User body, String query) throws ApiException {
    Object localVarPostBody = body;
    
    // verify the required parameter 'body' is set
    if (body == null) {
      throw new ApiException(400, "Missing the required parameter 'body' when calling testBodyWithQueryParams");
    }
    
    // verify the required parameter 'query' is set
    if (query == null) {
      throw new ApiException(400, "Missing the required parameter 'query' when calling testBodyWithQueryParams");
    }
    
    // create path and map variables
    String localVarPath = "/fake/body-with-query-params";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("", "query", query));

    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    return apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }


  /**
   * To test \&quot;client\&quot; model
   * To test \&quot;client\&quot; model
   * @param body client model (required)
   * @return Client
   * @throws ApiException if fails to make API call
   */
  public Client testClientModel(Client body) throws ApiException {
    return testClientModelWithHttpInfo(body).getData();
      }


  /**
   * To test \&quot;client\&quot; model
   * To test \&quot;client\&quot; model
   * @param body client model (required)
   * @return ApiResponse&lt;Client&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Client> testClientModelWithHttpInfo(Client body) throws ApiException {
    Object localVarPostBody = body;
    
    // verify the required parameter 'body' is set
    if (body == null) {
      throw new ApiException(400, "Missing the required parameter 'body' when calling testClientModel");
    }
    
    // create path and map variables
    String localVarPath = "/fake";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    final String[] localVarAccepts = {
      "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<Client> localVarReturnType = new GenericType<Client>() {};
    return apiClient.invokeAPI(localVarPath, "PATCH", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  public static class TestEndpointParametersOptionals {
    
    public Integer integer() {
      return this.integer;
    }

    public TestEndpointParametersOptionals integer(Integer integer) {
      this.integer = integer;
      return this;
    }

    private Integer integer = null;
    
    public Integer int32() {
      return this.int32;
    }

    public TestEndpointParametersOptionals int32(Integer int32) {
      this.int32 = int32;
      return this;
    }

    private Integer int32 = null;
    
    public Long int64() {
      return this.int64;
    }

    public TestEndpointParametersOptionals int64(Long int64) {
      this.int64 = int64;
      return this;
    }

    private Long int64 = null;
    
    public Float _float() {
      return this._float;
    }

    public TestEndpointParametersOptionals _float(Float _float) {
      this._float = _float;
      return this;
    }

    private Float _float = null;
    
    public String string() {
      return this.string;
    }

    public TestEndpointParametersOptionals string(String string) {
      this.string = string;
      return this;
    }

    private String string = null;
    
    public byte[] binary() {
      return this.binary;
    }

    public TestEndpointParametersOptionals binary(byte[] binary) {
      this.binary = binary;
      return this;
    }

    private byte[] binary = null;
    
    public LocalDate date() {
      return this.date;
    }

    public TestEndpointParametersOptionals date(LocalDate date) {
      this.date = date;
      return this;
    }

    private LocalDate date = null;
    
    public OffsetDateTime dateTime() {
      return this.dateTime;
    }

    public TestEndpointParametersOptionals dateTime(OffsetDateTime dateTime) {
      this.dateTime = dateTime;
      return this;
    }

    private OffsetDateTime dateTime = null;
    
    public String password() {
      return this.password;
    }

    public TestEndpointParametersOptionals password(String password) {
      this.password = password;
      return this;
    }

    private String password = null;
    
    public String paramCallback() {
      return this.paramCallback;
    }

    public TestEndpointParametersOptionals paramCallback(String paramCallback) {
      this.paramCallback = paramCallback;
      return this;
    }

    private String paramCallback = null;
    
  }

  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param number None (required)
   * @param _double None (required)
   * @param patternWithoutDelimiter None (required)
   * @param _byte None (required)
   * @throws ApiException if fails to make API call
   */
  public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte) throws ApiException {
    testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, null, null, null, null, null, null, null, null, null, null);
  }

  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param number None (required)
   * @param _double None (required)
   * @param patternWithoutDelimiter None (required)
   * @param _byte None (required)
   * @param optionals An object containing the optional parameters for this API call.
   * @throws ApiException if fails to make API call
   */
  public void testEndpointParametersOpts(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, TestEndpointParametersOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new TestEndpointParametersOptionals();
    }
    testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, optionals.integer(), optionals.int32(), optionals.int64(), optionals._float(), optionals.string(), optionals.binary(), optionals.date(), optionals.dateTime(), optionals.password(), optionals.paramCallback());
  }

  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param number None (required)
   * @param _double None (required)
   * @param patternWithoutDelimiter None (required)
   * @param _byte None (required)
   * @param integer None (optional)
   * @param int32 None (optional)
   * @param int64 None (optional)
   * @param _float None (optional)
   * @param string None (optional)
   * @param binary None (optional)
   * @param date None (optional)
   * @param dateTime None (optional)
   * @param password None (optional)
   * @param paramCallback None (optional)
   * @throws ApiException if fails to make API call
   */
  public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, byte[] binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws ApiException {

    testEndpointParametersWithHttpInfo(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
  }

  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param number None (required)
   * @param _double None (required)
   * @param patternWithoutDelimiter None (required)
   * @param _byte None (required)
   * @return ApiResponse&lt;Void&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testEndpointParametersWithHttpInfo(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte) throws ApiException {
    return testEndpointParametersWithHttpInfo(number, _double, patternWithoutDelimiter, _byte, null, null, null, null, null, null, null, null, null, null);
  }

  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param number None (required)
   * @param _double None (required)
   * @param patternWithoutDelimiter None (required)
   * @param _byte None (required)
   * @param optionals An object containing the optional parameters for this API call.
   * @return ApiResponse&lt;Void&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testEndpointParametersOptsWithHttpInfo(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, TestEndpointParametersOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new TestEndpointParametersOptionals();
    }
    return testEndpointParametersWithHttpInfo(number, _double, patternWithoutDelimiter, _byte, optionals.integer(), optionals.int32(), optionals.int64(), optionals._float(), optionals.string(), optionals.binary(), optionals.date(), optionals.dateTime(), optionals.password(), optionals.paramCallback());
  }

  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param number None (required)
   * @param _double None (required)
   * @param patternWithoutDelimiter None (required)
   * @param _byte None (required)
   * @param integer None (optional)
   * @param int32 None (optional)
   * @param int64 None (optional)
   * @param _float None (optional)
   * @param string None (optional)
   * @param binary None (optional)
   * @param date None (optional)
   * @param dateTime None (optional)
   * @param password None (optional)
   * @param paramCallback None (optional)
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testEndpointParametersWithHttpInfo(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, byte[] binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'number' is set
    if (number == null) {
      throw new ApiException(400, "Missing the required parameter 'number' when calling testEndpointParameters");
    }
    
    // verify the required parameter '_double' is set
    if (_double == null) {
      throw new ApiException(400, "Missing the required parameter '_double' when calling testEndpointParameters");
    }
    
    // verify the required parameter 'patternWithoutDelimiter' is set
    if (patternWithoutDelimiter == null) {
      throw new ApiException(400, "Missing the required parameter 'patternWithoutDelimiter' when calling testEndpointParameters");
    }
    
    // verify the required parameter '_byte' is set
    if (_byte == null) {
      throw new ApiException(400, "Missing the required parameter '_byte' when calling testEndpointParameters");
    }
    
    // create path and map variables
    String localVarPath = "/fake";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    if (integer != null)
      localVarFormParams.put("integer", integer);
if (int32 != null)
      localVarFormParams.put("int32", int32);
if (int64 != null)
      localVarFormParams.put("int64", int64);
if (number != null)
      localVarFormParams.put("number", number);
if (_float != null)
      localVarFormParams.put("float", _float);
if (_double != null)
      localVarFormParams.put("double", _double);
if (string != null)
      localVarFormParams.put("string", string);
if (patternWithoutDelimiter != null)
      localVarFormParams.put("pattern_without_delimiter", patternWithoutDelimiter);
if (_byte != null)
      localVarFormParams.put("byte", _byte);
if (binary != null)
      localVarFormParams.put("binary", binary);
if (date != null)
      localVarFormParams.put("date", date);
if (dateTime != null)
      localVarFormParams.put("dateTime", dateTime);
if (password != null)
      localVarFormParams.put("password", password);
if (paramCallback != null)
      localVarFormParams.put("callback", paramCallback);

    final String[] localVarAccepts = {
      "application/xml; charset=utf-8", "application/json; charset=utf-8"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/xml; charset=utf-8", "application/json; charset=utf-8"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "http_basic_test" };


    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  public static class TestEnumParametersOptionals {
    
    public List<String> enumFormStringArray() {
      return this.enumFormStringArray;
    }

    public TestEnumParametersOptionals enumFormStringArray(List<String> enumFormStringArray) {
      this.enumFormStringArray = enumFormStringArray;
      return this;
    }

    private List<String> enumFormStringArray = null;
    
    public String enumFormString() {
      return this.enumFormString;
    }

    public TestEnumParametersOptionals enumFormString(String enumFormString) {
      this.enumFormString = enumFormString;
      return this;
    }

    private String enumFormString = null;
    
    public List<String> enumHeaderStringArray() {
      return this.enumHeaderStringArray;
    }

    public TestEnumParametersOptionals enumHeaderStringArray(List<String> enumHeaderStringArray) {
      this.enumHeaderStringArray = enumHeaderStringArray;
      return this;
    }

    private List<String> enumHeaderStringArray = null;
    
    public String enumHeaderString() {
      return this.enumHeaderString;
    }

    public TestEnumParametersOptionals enumHeaderString(String enumHeaderString) {
      this.enumHeaderString = enumHeaderString;
      return this;
    }

    private String enumHeaderString = null;
    
    public List<String> enumQueryStringArray() {
      return this.enumQueryStringArray;
    }

    public TestEnumParametersOptionals enumQueryStringArray(List<String> enumQueryStringArray) {
      this.enumQueryStringArray = enumQueryStringArray;
      return this;
    }

    private List<String> enumQueryStringArray = null;
    
    public String enumQueryString() {
      return this.enumQueryString;
    }

    public TestEnumParametersOptionals enumQueryString(String enumQueryString) {
      this.enumQueryString = enumQueryString;
      return this;
    }

    private String enumQueryString = null;
    
    public Integer enumQueryInteger() {
      return this.enumQueryInteger;
    }

    public TestEnumParametersOptionals enumQueryInteger(Integer enumQueryInteger) {
      this.enumQueryInteger = enumQueryInteger;
      return this;
    }

    private Integer enumQueryInteger = null;
    
    public Double enumQueryDouble() {
      return this.enumQueryDouble;
    }

    public TestEnumParametersOptionals enumQueryDouble(Double enumQueryDouble) {
      this.enumQueryDouble = enumQueryDouble;
      return this;
    }

    private Double enumQueryDouble = null;
    
  }

  /**
   * To test enum parameters
   * To test enum parameters
   * @throws ApiException if fails to make API call
   */
  public void testEnumParameters() throws ApiException {
    testEnumParameters(null, null, null, null, null, null, null, null);
  }

  /**
   * To test enum parameters
   * To test enum parameters
   * @param optionals An object containing the optional parameters for this API call.
   * @throws ApiException if fails to make API call
   */
  public void testEnumParametersOpts(TestEnumParametersOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new TestEnumParametersOptionals();
    }
    testEnumParameters(optionals.enumFormStringArray(), optionals.enumFormString(), optionals.enumHeaderStringArray(), optionals.enumHeaderString(), optionals.enumQueryStringArray(), optionals.enumQueryString(), optionals.enumQueryInteger(), optionals.enumQueryDouble());
  }

  /**
   * To test enum parameters
   * To test enum parameters
   * @param enumFormStringArray Form parameter enum test (string array) (optional)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @throws ApiException if fails to make API call
   */
  public void testEnumParameters(List<String> enumFormStringArray, String enumFormString, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble) throws ApiException {

    testEnumParametersWithHttpInfo(enumFormStringArray, enumFormString, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble);
  }

  /**
   * To test enum parameters
   * To test enum parameters
   * @return ApiResponse&lt;Void&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testEnumParametersWithHttpInfo() throws ApiException {
    return testEnumParametersWithHttpInfo(null, null, null, null, null, null, null, null);
  }

  /**
   * To test enum parameters
   * To test enum parameters
   * @param optionals An object containing the optional parameters for this API call.
   * @return ApiResponse&lt;Void&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testEnumParametersOptsWithHttpInfo(TestEnumParametersOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new TestEnumParametersOptionals();
    }
    return testEnumParametersWithHttpInfo(optionals.enumFormStringArray(), optionals.enumFormString(), optionals.enumHeaderStringArray(), optionals.enumHeaderString(), optionals.enumQueryStringArray(), optionals.enumQueryString(), optionals.enumQueryInteger(), optionals.enumQueryDouble());
  }

  /**
   * To test enum parameters
   * To test enum parameters
   * @param enumFormStringArray Form parameter enum test (string array) (optional)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testEnumParametersWithHttpInfo(List<String> enumFormStringArray, String enumFormString, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/fake";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("csv", "enum_query_string_array", enumQueryStringArray));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_string", enumQueryString));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_integer", enumQueryInteger));

    if (enumHeaderStringArray != null)
      localVarHeaderParams.put("enum_header_string_array", apiClient.parameterToString(enumHeaderStringArray));
if (enumHeaderString != null)
      localVarHeaderParams.put("enum_header_string", apiClient.parameterToString(enumHeaderString));

    if (enumFormStringArray != null)
      localVarFormParams.put("enum_form_string_array", enumFormStringArray);
if (enumFormString != null)
      localVarFormParams.put("enum_form_string", enumFormString);
if (enumQueryDouble != null)
      localVarFormParams.put("enum_query_double", enumQueryDouble);

    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "*/*"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }


  /**
   * test inline additionalProperties
   * 
   * @param param request body (required)
   * @throws ApiException if fails to make API call
   */
  public void testInlineAdditionalProperties(Object param) throws ApiException {

    testInlineAdditionalPropertiesWithHttpInfo(param);
  }


  /**
   * test inline additionalProperties
   * 
   * @param param request body (required)
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testInlineAdditionalPropertiesWithHttpInfo(Object param) throws ApiException {
    Object localVarPostBody = param;
    
    // verify the required parameter 'param' is set
    if (param == null) {
      throw new ApiException(400, "Missing the required parameter 'param' when calling testInlineAdditionalProperties");
    }
    
    // create path and map variables
    String localVarPath = "/fake/inline-additionalProperties";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }


  /**
   * test json serialization of form data
   * 
   * @param param field1 (required)
   * @param param2 field2 (required)
   * @throws ApiException if fails to make API call
   */
  public void testJsonFormData(String param, String param2) throws ApiException {

    testJsonFormDataWithHttpInfo(param, param2);
  }


  /**
   * test json serialization of form data
   * 
   * @param param field1 (required)
   * @param param2 field2 (required)
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testJsonFormDataWithHttpInfo(String param, String param2) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'param' is set
    if (param == null) {
      throw new ApiException(400, "Missing the required parameter 'param' when calling testJsonFormData");
    }
    
    // verify the required parameter 'param2' is set
    if (param2 == null) {
      throw new ApiException(400, "Missing the required parameter 'param2' when calling testJsonFormData");
    }
    
    // create path and map variables
    String localVarPath = "/fake/jsonFormData";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    if (param != null)
      localVarFormParams.put("param", param);
if (param2 != null)
      localVarFormParams.put("param2", param2);

    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
}
