package io.swagger.client.api;

import io.swagger.client.ApiClient;

import java.math.BigDecimal;
import io.swagger.client.model.Client;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;
import io.swagger.client.model.OuterComposite;
import io.swagger.client.model.User;

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
    * @return ResponseEntity&lt;Boolean&gt;
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public ResponseEntity<Boolean> fakeOuterBooleanSerializeWithHttpInfo() throws RestClientException {
        return fakeOuterBooleanSerializeWithHttpInfo(null);
    }

    /**
    * 
    * Test serialization of outer boolean types
    * @param optionals An object containing the optional parameters for this API call.
    * @return ResponseEntity&lt;Boolean&gt;
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public ResponseEntity<Boolean> fakeOuterBooleanSerializeOptsWithHttpInfo(FakeOuterBooleanSerializeOptionals optionals) throws RestClientException {
        if (optionals == null) {
            optionals = new FakeOuterBooleanSerializeOptionals();
        }
        return fakeOuterBooleanSerializeWithHttpInfo(optionals.body());
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

        final String[] accepts = { };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { };
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
    * @return ResponseEntity&lt;OuterComposite&gt;
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public ResponseEntity<OuterComposite> fakeOuterCompositeSerializeWithHttpInfo() throws RestClientException {
        return fakeOuterCompositeSerializeWithHttpInfo(null);
    }

    /**
    * 
    * Test serialization of object with outer number type
    * @param optionals An object containing the optional parameters for this API call.
    * @return ResponseEntity&lt;OuterComposite&gt;
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public ResponseEntity<OuterComposite> fakeOuterCompositeSerializeOptsWithHttpInfo(FakeOuterCompositeSerializeOptionals optionals) throws RestClientException {
        if (optionals == null) {
            optionals = new FakeOuterCompositeSerializeOptionals();
        }
        return fakeOuterCompositeSerializeWithHttpInfo(optionals.body());
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

        final String[] accepts = { };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { };
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
    * @return ResponseEntity&lt;BigDecimal&gt;
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public ResponseEntity<BigDecimal> fakeOuterNumberSerializeWithHttpInfo() throws RestClientException {
        return fakeOuterNumberSerializeWithHttpInfo(null);
    }

    /**
    * 
    * Test serialization of outer number types
    * @param optionals An object containing the optional parameters for this API call.
    * @return ResponseEntity&lt;BigDecimal&gt;
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public ResponseEntity<BigDecimal> fakeOuterNumberSerializeOptsWithHttpInfo(FakeOuterNumberSerializeOptionals optionals) throws RestClientException {
        if (optionals == null) {
            optionals = new FakeOuterNumberSerializeOptionals();
        }
        return fakeOuterNumberSerializeWithHttpInfo(optionals.body());
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

        final String[] accepts = { };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { };
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
    * @return ResponseEntity&lt;String&gt;
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public ResponseEntity<String> fakeOuterStringSerializeWithHttpInfo() throws RestClientException {
        return fakeOuterStringSerializeWithHttpInfo(null);
    }

    /**
    * 
    * Test serialization of outer string types
    * @param optionals An object containing the optional parameters for this API call.
    * @return ResponseEntity&lt;String&gt;
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public ResponseEntity<String> fakeOuterStringSerializeOptsWithHttpInfo(FakeOuterStringSerializeOptionals optionals) throws RestClientException {
        if (optionals == null) {
            optionals = new FakeOuterStringSerializeOptionals();
        }
        return fakeOuterStringSerializeWithHttpInfo(optionals.body());
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

        final String[] accepts = { };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<String> returnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }


    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param body  (required)
     * @param query  (required)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void testBodyWithQueryParams(User body, String query) throws RestClientException {
        testBodyWithQueryParamsWithHttpInfo(body, query);
    }


    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param body  (required)
     * @param query  (required)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testBodyWithQueryParamsWithHttpInfo(User body, String query) throws RestClientException {
        Object postBody = body;
        
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'body' when calling testBodyWithQueryParams");
        }
        
        // verify the required parameter 'query' is set
        if (query == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'query' when calling testBodyWithQueryParams");
        }
        
        String path = UriComponentsBuilder.fromPath("/fake/body-with-query-params").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "query", query));

        final String[] accepts = { };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/json"
        };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI(path, HttpMethod.PUT, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
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
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte) throws RestClientException {
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
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public void testEndpointParametersOpts(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, TestEndpointParametersOptionals optionals) throws RestClientException {
        if (optionals == null) {
            optionals = new TestEndpointParametersOptionals();
        }
        testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, optionals.integer(), optionals.int32(), optionals.int64(), optionals._float(), optionals.string(), optionals.binary(), optionals.date(), optionals.dateTime(), optionals.password(), optionals.paramCallback());
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
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
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, byte[] binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws RestClientException {
        testEndpointParametersWithHttpInfo(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
    }

    /**
    * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    * @param number None (required)
    * @param _double None (required)
    * @param patternWithoutDelimiter None (required)
    * @param _byte None (required)
    * @return ResponseEntity&lt;Void&gt;
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public ResponseEntity<Void> testEndpointParametersWithHttpInfo(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte) throws RestClientException {
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
    * @return ResponseEntity&lt;Void&gt;
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public ResponseEntity<Void> testEndpointParametersOptsWithHttpInfo(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, TestEndpointParametersOptionals optionals) throws RestClientException {
        if (optionals == null) {
            optionals = new TestEndpointParametersOptionals();
        }
        return testEndpointParametersWithHttpInfo(number, _double, patternWithoutDelimiter, _byte, optionals.integer(), optionals.int32(), optionals.int64(), optionals._float(), optionals.string(), optionals.binary(), optionals.date(), optionals.dateTime(), optionals.password(), optionals.paramCallback());
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
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
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testEndpointParametersWithHttpInfo(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, byte[] binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws RestClientException {
        Object postBody = null;
        
        // verify the required parameter 'number' is set
        if (number == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'number' when calling testEndpointParameters");
        }
        
        // verify the required parameter '_double' is set
        if (_double == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter '_double' when calling testEndpointParameters");
        }
        
        // verify the required parameter 'patternWithoutDelimiter' is set
        if (patternWithoutDelimiter == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'patternWithoutDelimiter' when calling testEndpointParameters");
        }
        
        // verify the required parameter '_byte' is set
        if (_byte == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter '_byte' when calling testEndpointParameters");
        }
        
        String path = UriComponentsBuilder.fromPath("/fake").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        if (integer != null)
            formParams.add("integer", integer);
        if (int32 != null)
            formParams.add("int32", int32);
        if (int64 != null)
            formParams.add("int64", int64);
        if (number != null)
            formParams.add("number", number);
        if (_float != null)
            formParams.add("float", _float);
        if (_double != null)
            formParams.add("double", _double);
        if (string != null)
            formParams.add("string", string);
        if (patternWithoutDelimiter != null)
            formParams.add("pattern_without_delimiter", patternWithoutDelimiter);
        if (_byte != null)
            formParams.add("byte", _byte);
        if (binary != null)
            formParams.add("binary", binary);
        if (date != null)
            formParams.add("date", date);
        if (dateTime != null)
            formParams.add("dateTime", dateTime);
        if (password != null)
            formParams.add("password", password);
        if (paramCallback != null)
            formParams.add("callback", paramCallback);

        final String[] accepts = { 
            "application/xml; charset=utf-8", "application/json; charset=utf-8"
        };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/xml; charset=utf-8", "application/json; charset=utf-8"
        };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] { "http_basic_test" };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
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
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public void testEnumParameters() throws RestClientException {
        testEnumParameters(null, null, null, null, null, null, null, null);
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
        testEnumParameters(optionals.enumFormStringArray(), optionals.enumFormString(), optionals.enumHeaderStringArray(), optionals.enumHeaderString(), optionals.enumQueryStringArray(), optionals.enumQueryString(), optionals.enumQueryInteger(), optionals.enumQueryDouble());
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param enumFormStringArray Form parameter enum test (string array) (optional)
     * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
     * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
     * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
     * @param enumQueryStringArray Query parameter enum test (string array) (optional)
     * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
     * @param enumQueryInteger Query parameter enum test (double) (optional)
     * @param enumQueryDouble Query parameter enum test (double) (optional)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void testEnumParameters(List<String> enumFormStringArray, String enumFormString, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble) throws RestClientException {
        testEnumParametersWithHttpInfo(enumFormStringArray, enumFormString, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble);
    }

    /**
    * To test enum parameters
    * To test enum parameters
    * @return ResponseEntity&lt;Void&gt;
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public ResponseEntity<Void> testEnumParametersWithHttpInfo() throws RestClientException {
        return testEnumParametersWithHttpInfo(null, null, null, null, null, null, null, null);
    }

    /**
    * To test enum parameters
    * To test enum parameters
    * @param optionals An object containing the optional parameters for this API call.
    * @return ResponseEntity&lt;Void&gt;
    * @throws RestClientException if an error occurs while attempting to invoke the API
    */
    public ResponseEntity<Void> testEnumParametersOptsWithHttpInfo(TestEnumParametersOptionals optionals) throws RestClientException {
        if (optionals == null) {
            optionals = new TestEnumParametersOptionals();
        }
        return testEnumParametersWithHttpInfo(optionals.enumFormStringArray(), optionals.enumFormString(), optionals.enumHeaderStringArray(), optionals.enumHeaderString(), optionals.enumQueryStringArray(), optionals.enumQueryString(), optionals.enumQueryInteger(), optionals.enumQueryDouble());
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param enumFormStringArray Form parameter enum test (string array) (optional)
     * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
     * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
     * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
     * @param enumQueryStringArray Query parameter enum test (string array) (optional)
     * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
     * @param enumQueryInteger Query parameter enum test (double) (optional)
     * @param enumQueryDouble Query parameter enum test (double) (optional)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testEnumParametersWithHttpInfo(List<String> enumFormStringArray, String enumFormString, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble) throws RestClientException {
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

        if (enumFormStringArray != null)
            formParams.add("enum_form_string_array", enumFormStringArray);
        if (enumFormString != null)
            formParams.add("enum_form_string", enumFormString);
        if (enumQueryDouble != null)
            formParams.add("enum_query_double", enumQueryDouble);

        final String[] accepts = { 
            "*/*"
        };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "*/*"
        };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI(path, HttpMethod.GET, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }


    /**
     * test inline additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param param request body (required)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void testInlineAdditionalProperties(Object param) throws RestClientException {
        testInlineAdditionalPropertiesWithHttpInfo(param);
    }


    /**
     * test inline additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param param request body (required)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testInlineAdditionalPropertiesWithHttpInfo(Object param) throws RestClientException {
        Object postBody = param;
        
        // verify the required parameter 'param' is set
        if (param == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'param' when calling testInlineAdditionalProperties");
        }
        
        String path = UriComponentsBuilder.fromPath("/fake/inline-additionalProperties").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { };
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
     * @param param field1 (required)
     * @param param2 field2 (required)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void testJsonFormData(String param, String param2) throws RestClientException {
        testJsonFormDataWithHttpInfo(param, param2);
    }


    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param param field1 (required)
     * @param param2 field2 (required)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testJsonFormDataWithHttpInfo(String param, String param2) throws RestClientException {
        Object postBody = null;
        
        // verify the required parameter 'param' is set
        if (param == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'param' when calling testJsonFormData");
        }
        
        // verify the required parameter 'param2' is set
        if (param2 == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'param2' when calling testJsonFormData");
        }
        
        String path = UriComponentsBuilder.fromPath("/fake/jsonFormData").build().toUriString();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        if (param != null)
            formParams.add("param", param);
        if (param2 != null)
            formParams.add("param2", param2);

        final String[] accepts = { };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/json"
        };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI(path, HttpMethod.GET, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
}
