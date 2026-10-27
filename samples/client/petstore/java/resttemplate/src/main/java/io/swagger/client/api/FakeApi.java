package io.swagger.client.api;

import io.swagger.client.ApiClient;

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

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.util.UriComponentsBuilder;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

@Component("io.swagger.client.api.FakeApi")
public class FakeApi {
    private ApiClient apiClient;

    public FakeApi() {
        this(new ApiClient());
    }

    @Autowired
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
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public Boolean fakeOuterBooleanSerialize() throws RestClientException {
        return fakeOuterBooleanSerialize(null);
    }

    /**
    * 
    * Test serialization of outer boolean types
    * @param optionals An object containing the optional parameters for this API call.
    * @return Boolean
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public Boolean fakeOuterBooleanSerializeOpts(FakeOuterBooleanSerializeOptionals optionals) throws RestClientException {
        if (optionals == null) {
            optionals = new FakeOuterBooleanSerializeOptionals();
        }
        return fakeOuterBooleanSerialize(optionals.body());
    }

    /**
     * 
     * Test serialization of outer boolean types
     * <p><b>200</b> - Output boolean
     * @param body Input boolean as post body (optional)
     * @return Boolean
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public Boolean fakeOuterBooleanSerialize(Boolean body) throws RestClientException {
        return fakeOuterBooleanSerializeWithHttpInfo(body).getBody();
    }

    /**
     * 
     * Test serialization of outer boolean types
     * <p><b>200</b> - Output boolean
     * @param body Input boolean as post body (optional)
     * @return ResponseEntity&lt;Boolean&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Boolean> fakeOuterBooleanSerializeWithHttpInfo(Boolean body) throws RestClientException {
        Object postBody = body;
        String path = UriComponentsBuilder.fromPath("/fake/outer/boolean").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { 
            "*/*"
         };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "*/*"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Boolean> returnType = new ParameterizedTypeReference<Boolean>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
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
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public OuterComposite fakeOuterCompositeSerialize() throws RestClientException {
        return fakeOuterCompositeSerialize(null);
    }

    /**
    * 
    * Test serialization of object with outer number type
    * @param optionals An object containing the optional parameters for this API call.
    * @return OuterComposite
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public OuterComposite fakeOuterCompositeSerializeOpts(FakeOuterCompositeSerializeOptionals optionals) throws RestClientException {
        if (optionals == null) {
            optionals = new FakeOuterCompositeSerializeOptionals();
        }
        return fakeOuterCompositeSerialize(optionals.body());
    }

    /**
     * 
     * Test serialization of object with outer number type
     * <p><b>200</b> - Output composite
     * @param body Input composite as post body (optional)
     * @return OuterComposite
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public OuterComposite fakeOuterCompositeSerialize(OuterComposite body) throws RestClientException {
        return fakeOuterCompositeSerializeWithHttpInfo(body).getBody();
    }

    /**
     * 
     * Test serialization of object with outer number type
     * <p><b>200</b> - Output composite
     * @param body Input composite as post body (optional)
     * @return ResponseEntity&lt;OuterComposite&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<OuterComposite> fakeOuterCompositeSerializeWithHttpInfo(OuterComposite body) throws RestClientException {
        Object postBody = body;
        String path = UriComponentsBuilder.fromPath("/fake/outer/composite").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { 
            "*/*"
         };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "*/*"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<OuterComposite> returnType = new ParameterizedTypeReference<OuterComposite>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
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
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public BigDecimal fakeOuterNumberSerialize() throws RestClientException {
        return fakeOuterNumberSerialize(null);
    }

    /**
    * 
    * Test serialization of outer number types
    * @param optionals An object containing the optional parameters for this API call.
    * @return BigDecimal
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public BigDecimal fakeOuterNumberSerializeOpts(FakeOuterNumberSerializeOptionals optionals) throws RestClientException {
        if (optionals == null) {
            optionals = new FakeOuterNumberSerializeOptionals();
        }
        return fakeOuterNumberSerialize(optionals.body());
    }

    /**
     * 
     * Test serialization of outer number types
     * <p><b>200</b> - Output number
     * @param body Input number as post body (optional)
     * @return BigDecimal
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public BigDecimal fakeOuterNumberSerialize(BigDecimal body) throws RestClientException {
        return fakeOuterNumberSerializeWithHttpInfo(body).getBody();
    }

    /**
     * 
     * Test serialization of outer number types
     * <p><b>200</b> - Output number
     * @param body Input number as post body (optional)
     * @return ResponseEntity&lt;BigDecimal&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<BigDecimal> fakeOuterNumberSerializeWithHttpInfo(BigDecimal body) throws RestClientException {
        Object postBody = body;
        String path = UriComponentsBuilder.fromPath("/fake/outer/number").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { 
            "*/*"
         };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "*/*"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<BigDecimal> returnType = new ParameterizedTypeReference<BigDecimal>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
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
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public String fakeOuterStringSerialize() throws RestClientException {
        return fakeOuterStringSerialize(null);
    }

    /**
    * 
    * Test serialization of outer string types
    * @param optionals An object containing the optional parameters for this API call.
    * @return String
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public String fakeOuterStringSerializeOpts(FakeOuterStringSerializeOptionals optionals) throws RestClientException {
        if (optionals == null) {
            optionals = new FakeOuterStringSerializeOptionals();
        }
        return fakeOuterStringSerialize(optionals.body());
    }

    /**
     * 
     * Test serialization of outer string types
     * <p><b>200</b> - Output string
     * @param body Input string as post body (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String fakeOuterStringSerialize(String body) throws RestClientException {
        return fakeOuterStringSerializeWithHttpInfo(body).getBody();
    }

    /**
     * 
     * Test serialization of outer string types
     * <p><b>200</b> - Output string
     * @param body Input string as post body (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> fakeOuterStringSerializeWithHttpInfo(String body) throws RestClientException {
        Object postBody = body;
        String path = UriComponentsBuilder.fromPath("/fake/outer/string").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { 
            "*/*"
         };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "*/*"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<String> returnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }


    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * <p><b>200</b> - successful operation
     * @param body client model (required)
     * @return Client
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public Client testClientModel(Client body) throws RestClientException {
        return testClientModelWithHttpInfo(body).getBody();
    }

    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * <p><b>200</b> - successful operation
     * @param body client model (required)
     * @return ResponseEntity&lt;Client&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Client> testClientModelWithHttpInfo(Client body) throws RestClientException {
        Object postBody = body;
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'body' when calling testClientModel");
        }
        String path = UriComponentsBuilder.fromPath("/fake").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { 
            "application/json"
         };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/json"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Client> returnType = new ParameterizedTypeReference<Client>() {};
        return apiClient.invokeAPI(path, HttpMethod.PATCH, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }


    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param body  (required)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void testEndpointParameters(FakeBody body) throws RestClientException {
        testEndpointParametersWithHttpInfo(body);
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param body  (required)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testEndpointParametersWithHttpInfo(FakeBody body) throws RestClientException {
        Object postBody = body;
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'body' when calling testEndpointParameters");
        }
        String path = UriComponentsBuilder.fromPath("/fake").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = {  };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/xml; charset&#x3D;utf-8", "application/json; charset&#x3D;utf-8"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] { "http_basic_test" };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
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
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public void testEnumParameters() throws RestClientException {
        testEnumParameters(null, null, null, null, null);
    }

    /**
    * To test enum parameters
    * To test enum parameters
    * @param optionals An object containing the optional parameters for this API call.
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public void testEnumParametersOpts(TestEnumParametersOptionals optionals) throws RestClientException {
        if (optionals == null) {
            optionals = new TestEnumParametersOptionals();
        }
        testEnumParameters(optionals.enumHeaderStringArray(), optionals.enumHeaderString(), optionals.enumQueryStringArray(), optionals.enumQueryString(), optionals.enumQueryInteger());
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
     * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
     * @param enumQueryStringArray Query parameter enum test (string array) (optional)
     * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
     * @param enumQueryInteger Query parameter enum test (double) (optional)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger) throws RestClientException {
        testEnumParametersWithHttpInfo(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger);
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
     * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
     * @param enumQueryStringArray Query parameter enum test (string array) (optional)
     * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
     * @param enumQueryInteger Query parameter enum test (double) (optional)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testEnumParametersWithHttpInfo(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger) throws RestClientException {
        Object postBody = null;
        String path = UriComponentsBuilder.fromPath("/fake").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("csv".toUpperCase()), "enum_query_string_array", enumQueryStringArray));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "enum_query_string", enumQueryString));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "enum_query_integer", enumQueryInteger));
        if (enumHeaderStringArray != null)
            headerParams.add("enum_header_string_array", apiClient.parameterToString(enumHeaderStringArray));
        if (enumHeaderString != null)
            headerParams.add("enum_header_string", apiClient.parameterToString(enumHeaderString));

        final String[] accepts = {  };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = {  };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI(path, HttpMethod.GET, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
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
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public void testEnumRequestBody() throws RestClientException {
        testEnumRequestBody(null);
    }

    /**
    * To test enum parameters
    * To test enum parameters
    * @param optionals An object containing the optional parameters for this API call.
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public void testEnumRequestBodyOpts(TestEnumRequestBodyOptionals optionals) throws RestClientException {
        if (optionals == null) {
            optionals = new TestEnumRequestBodyOptionals();
        }
        testEnumRequestBody(optionals.body());
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param body  (optional)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void testEnumRequestBody(EnumFormBody body) throws RestClientException {
        testEnumRequestBodyWithHttpInfo(body);
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param body  (optional)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testEnumRequestBodyWithHttpInfo(EnumFormBody body) throws RestClientException {
        Object postBody = body;
        String path = UriComponentsBuilder.fromPath("/fake/enum/form").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = {  };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "*/*"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }


    /**
     * test inline additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param body request body (required)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void testInlineAdditionalProperties(Map<String, String> body) throws RestClientException {
        testInlineAdditionalPropertiesWithHttpInfo(body);
    }

    /**
     * test inline additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param body request body (required)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testInlineAdditionalPropertiesWithHttpInfo(Map<String, String> body) throws RestClientException {
        Object postBody = body;
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'body' when calling testInlineAdditionalProperties");
        }
        String path = UriComponentsBuilder.fromPath("/fake/inline-additionalProperties").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = {  };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/json"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param body  (required)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void testJsonFormData(FakeJsonFormDataBody body) throws RestClientException {
        testJsonFormDataWithHttpInfo(body);
    }
  
    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param body  (required)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testJsonFormDataWithHttpInfo(FakeJsonFormDataBody body) throws RestClientException {
        Object postBody = body;
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'body' when calling testJsonFormData");
        }
        String path = UriComponentsBuilder.fromPath("/fake/jsonFormData").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = {  };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/json"
         };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
}
