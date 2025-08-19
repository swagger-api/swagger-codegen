package io.swagger.client.api;

import io.swagger.client.ApiException;
import io.swagger.client.ApiClient;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;

import javax.ws.rs.core.GenericType;

import java.math.BigDecimal;
import io.swagger.client.model.Client;
import io.swagger.client.model.EnumFormBody;
import io.swagger.client.model.FakeBody;
import io.swagger.client.model.FakeBody1;
import io.swagger.client.model.FakeJsonFormDataBody;
import io.swagger.client.model.OuterComposite;

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
    Object localVarPostBody = body;
    // create path and map variables
    String localVarPath = "/fake/outer/boolean";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();



    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "*/*"
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
    Object localVarPostBody = body;
    // create path and map variables
    String localVarPath = "/fake/outer/composite";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();



    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "*/*"
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
    Object localVarPostBody = body;
    // create path and map variables
    String localVarPath = "/fake/outer/number";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();



    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "*/*"
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
    Object localVarPostBody = body;
    // create path and map variables
    String localVarPath = "/fake/outer/string";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();



    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "*/*"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }


  /**
   * To test \&quot;client\&quot; model
   * To test \&quot;client\&quot; model
   * @param body client model (required)
   * @return Client
   * @throws ApiException if fails to make API call
   */
  public Client testClientModel(Client body) throws ApiException {
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


  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param body  (required)
   * @throws ApiException if fails to make API call
   */
  public void testEndpointParameters(FakeBody body) throws ApiException {
    Object localVarPostBody = body;
    // verify the required parameter 'body' is set
    if (body == null) {
      throw new ApiException(400, "Missing the required parameter 'body' when calling testEndpointParameters");
    }
    // create path and map variables
    String localVarPath = "/fake";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();



    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/xml; charset=utf-8", "application/json; charset=utf-8"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "http_basic_test" };

    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  public static class TestEnumParametersOptionals {
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
  }

  /**
   * To test enum parameters
   * To test enum parameters
   * @throws ApiException if fails to make API call
   */
  public void testEnumParameters() throws ApiException {
    testEnumParameters(null, null, null, null, null);
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
    testEnumParameters(optionals.enumHeaderStringArray(), optionals.enumHeaderString(), optionals.enumQueryStringArray(), optionals.enumQueryString(), optionals.enumQueryInteger());
  }

  /**
   * To test enum parameters
   * To test enum parameters
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @throws ApiException if fails to make API call
   */
  public void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger) throws ApiException {
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

    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  public static class TestEnumRequestBodyOptionals {
    public Body4 body() {
      return this.body;
    }

    public TestEnumRequestBodyOptionals body(Body4 body) {
      this.body = body;
      return this;
    }

    private Body4 body = null;
  }

  /**
   * To test enum parameters
   * To test enum parameters
   * @throws ApiException if fails to make API call
   */
  public void testEnumRequestBody() throws ApiException {
    testEnumRequestBody(null);
  }

  /**
   * To test enum parameters
   * To test enum parameters
   * @param optionals An object containing the optional parameters for this API call.
   * @throws ApiException if fails to make API call
   */
  public void testEnumRequestBodyOpts(TestEnumRequestBodyOptionals optionals) throws ApiException {
    if (optionals == null) {
      optionals = new TestEnumRequestBodyOptionals();
    }
    testEnumRequestBody(optionals.body());
  }

  /**
   * To test enum parameters
   * To test enum parameters
   * @param body  (optional)
   * @throws ApiException if fails to make API call
   */
  public void testEnumRequestBody(EnumFormBody body) throws ApiException {
    Object localVarPostBody = body;
    // create path and map variables
    String localVarPath = "/fake/enum/form";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();



    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "*/*"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }


  /**
   * test inline additionalProperties
   * 
   * @param body request body (required)
   * @throws ApiException if fails to make API call
   */
  public void testInlineAdditionalProperties(Map<String, String> body) throws ApiException {
    Object localVarPostBody = body;
    // verify the required parameter 'body' is set
    if (body == null) {
      throw new ApiException(400, "Missing the required parameter 'body' when calling testInlineAdditionalProperties");
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

    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }


  /**
   * test json serialization of form data
   * 
   * @param body  (required)
   * @throws ApiException if fails to make API call
   */
  public void testJsonFormData(FakeJsonFormDataBody body) throws ApiException {
    Object localVarPostBody = body;
    // verify the required parameter 'body' is set
    if (body == null) {
      throw new ApiException(400, "Missing the required parameter 'body' when calling testJsonFormData");
    }
    // create path and map variables
    String localVarPath = "/fake/jsonFormData";

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

    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
}
