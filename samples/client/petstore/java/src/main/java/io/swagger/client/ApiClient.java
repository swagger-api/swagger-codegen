package io.swagger.client;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonSyntaxException;
import com.google.gson.stream.JsonReader;

import com.squareup.okhttp.Call;
import com.squareup.okhttp.Callback;
import com.squareup.okhttp.OkHttpClient;
import com.squareup.okhttp.Request;
import com.squareup.okhttp.Response;
import com.squareup.okhttp.RequestBody;
import com.squareup.okhttp.FormEncodingBuilder;
import com.squareup.okhttp.MultipartBuilder;
import com.squareup.okhttp.MediaType;
import com.squareup.okhttp.Headers;

import java.lang.reflect.Type;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Map.Entry;
import java.util.HashMap;
import java.util.List;
import java.util.Date;
import java.util.TimeZone;

import java.net.URLEncoder;
import java.net.URLConnection;

import java.io.File;
import java.io.StringReader;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.text.ParseException;

import io.swagger.client.auth.Authentication;
import io.swagger.client.auth.HttpBasicAuth;
import io.swagger.client.auth.ApiKeyAuth;
import io.swagger.client.auth.OAuth;

public class ApiClient {
  private String basePath = "http://petstore.swagger.io/v2";
  private boolean lenientOnJson = false;
  private boolean debugging = false;
  private Map<String, String> defaultHeaderMap = new HashMap<String, String>();

  private Map<String, Authentication> authentications;

  private DateFormat dateFormat;

  private OkHttpClient httpClient;
  private Gson gson;

  public ApiClient() {
    // Use ISO 8601 format for date and datetime.
    // See https://en.wikipedia.org/wiki/ISO_8601
    dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");

    // Use UTC as the default time zone.
    dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));

    // Set default User-Agent.
    setUserAgent("Java-Swagger");

    httpClient = new OkHttpClient();

    gson = new GsonBuilder()
      .serializeNulls()
      .setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
      .create();

    // Setup authentications (key: authentication name, value: authentication).
    authentications = new HashMap<String, Authentication>();
    authentications.put("api_key", new ApiKeyAuth("header", "api_key"));
    authentications.put("petstore_auth", new OAuth());
    // Prevent the authentications from being modified.
    authentications = Collections.unmodifiableMap(authentications);
  }

  public String getBasePath() {
    return basePath;
  }

  public ApiClient setBasePath(String basePath) {
    this.basePath = basePath;
    return this;
  }

  public OkHttpClient getHttpClient() {
    return httpClient;
  }

  public ApiClient setHttpClient(OkHttpClient httpClient) {
    this.httpClient = httpClient;
    return this;
  }

  public Gson getGson() {
    return gson;
  }

  public ApiClient setGson(Gson gson) {
    this.gson = gson;
    return this;
  }

  /**
   * Get authentications (key: authentication name, value: authentication).
   */
  public Map<String, Authentication> getAuthentications() {
    return authentications;
  }

  /**
   * Get authentication for the given name.
   *
   * @param authName The authentication name
   * @return The authentication, null if not found
   */
  public Authentication getAuthentication(String authName) {
    return authentications.get(authName);
  }

  /**
   * Helper method to set username for the first HTTP basic authentication.
   */
  public void setUsername(String username) {
    for (Authentication auth : authentications.values()) {
      if (auth instanceof HttpBasicAuth) {
        ((HttpBasicAuth) auth).setUsername(username);
        return;
      }
    }
    throw new RuntimeException("No HTTP basic authentication configured!");
  }

  /**
   * Helper method to set password for the first HTTP basic authentication.
   */
  public void setPassword(String password) {
    for (Authentication auth : authentications.values()) {
      if (auth instanceof HttpBasicAuth) {
        ((HttpBasicAuth) auth).setPassword(password);
        return;
      }
    }
    throw new RuntimeException("No HTTP basic authentication configured!");
  }

  /**
   * Helper method to set API key value for the first API key authentication.
   */
  public void setApiKey(String apiKey) {
    for (Authentication auth : authentications.values()) {
      if (auth instanceof ApiKeyAuth) {
        ((ApiKeyAuth) auth).setApiKey(apiKey);
        return;
      }
    }
    throw new RuntimeException("No API key authentication configured!");
  }

  /**
   * Helper method to set API key prefix for the first API key authentication.
   */
  public void setApiKeyPrefix(String apiKeyPrefix) {
    for (Authentication auth : authentications.values()) {
      if (auth instanceof ApiKeyAuth) {
        ((ApiKeyAuth) auth).setApiKeyPrefix(apiKeyPrefix);
        return;
      }
    }
    throw new RuntimeException("No API key authentication configured!");
  }

  /**
   * Set the User-Agent header's value (by adding to the default header map).
   */
  public ApiClient setUserAgent(String userAgent) {
    addDefaultHeader("User-Agent", userAgent);
    return this;
  }

  /**
   * Add a default header.
   *
   * @param key The header's key
   * @param value The header's value
   */
  public ApiClient addDefaultHeader(String key, String value) {
    defaultHeaderMap.put(key, value);
    return this;
  }

  /**
   * @see https://google-gson.googlecode.com/svn/trunk/gson/docs/javadocs/com/google/gson/stream/JsonReader.html#setLenient(boolean)
   */
  public boolean isLenientOnJson() {
    return lenientOnJson;
  }

  public ApiClient setLenientOnJson(boolean lenient) {
    this.lenientOnJson = lenient;
    return this;
  }

  /**
   * Check that whether debugging is enabled for this API client.
   */
  public boolean isDebugging() {
    return debugging;
  }

  /**
   * Enable/disable debugging for this API client.
   *
   * @param debugging To enable (true) or disable (false) debugging
   */
  public ApiClient setDebugging(boolean debugging) {
    this.debugging = debugging;
    return this;
  }

  /**
   * Get the date format used to parse/format date parameters.
   */
  public DateFormat getDateFormat() {
    return dateFormat;
  }

  /**
   * Set the date format used to parse/format date parameters.
   */
  public ApiClient getDateFormat(DateFormat dateFormat) {
    this.dateFormat = dateFormat;
    return this;
  }

  /**
   * Parse the given string into Date object.
   */
  public Date parseDate(String str) {
    try {
      return dateFormat.parse(str);
    } catch (java.text.ParseException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Format the given Date object into string.
   */
  public String formatDate(Date date) {
    return dateFormat.format(date);
  }

  /**
   * Format the given parameter object into string.
   */
  public String parameterToString(Object param) {
    if (param == null) {
      return "";
    } else if (param instanceof Date) {
      return formatDate((Date) param);
    } else if (param instanceof Collection) {
      StringBuilder b = new StringBuilder();
      for (Object o : (Collection)param) {
        if (b.length() > 0) {
          b.append(",");
        }
        b.append(String.valueOf(o));
      }
      return b.toString();
    } else {
      return String.valueOf(param);
    }
  }

  /**
   * Select the Accept header's value from the given accepts array:
   *   if JSON exists in the given array, use it;
   *   otherwise use all of them (joining into a string)
   *
   * @param accepts The accepts array to select from
   * @return The Accept header to use. If the given array is empty,
   *   null will be returned (not to set the Accept header explicitly).
   */
  public String selectHeaderAccept(String[] accepts) {
    if (accepts.length == 0) return null;
    if (StringUtil.containsIgnoreCase(accepts, "application/json")) return "application/json";
    return StringUtil.join(accepts, ",");
  }

  /**
   * Select the Content-Type header's value from the given array:
   *   if JSON exists in the given array, use it;
   *   otherwise use the first one of the array.
   *
   * @param contentTypes The Content-Type array to select from
   * @return The Content-Type header to use. If the given array is empty,
   *   JSON will be used.
   */
  public String selectHeaderContentType(String[] contentTypes) {
    if (contentTypes.length == 0) return "application/json";
    if (StringUtil.containsIgnoreCase(contentTypes, "application/json")) return "application/json";
    return contentTypes[0];
  }

  /**
   * Escape the given string to be used as URL query value.
   */
  public String escapeString(String str) {
    try {
      return URLEncoder.encode(str, "utf8").replaceAll("\\+", "%20");
    } catch (UnsupportedEncodingException e) {
      return str;
    }
  }

  /**
   * Deserialize response body to Java object, according the Content-Type
   * response header.
   *
   * @param response HTTP response
   * @param returnType The type of the Java object
   * @return The deserialized Java object
   */
  public <T> T deserialize(Response response, Type returnType) throws ApiException {
    if (response == null || returnType == null)
      return null;

    String respBody;
    try {
      if (response.body() != null)
        respBody = response.body().string();
      else
        respBody = null;
    } catch (IOException e) {
      throw new ApiException(e);
    }

    if (respBody == null || "".equals(respBody))
      return null;

    String contentType = response.headers().get("Content-Type");
    if (contentType == null) {
      // ensuring a default content type
      contentType = "application/json";
    }
    if (contentType.startsWith("application/json")) {
      try {
        if (lenientOnJson) {
          JsonReader jsonReader = new JsonReader(new StringReader(respBody));
          // see https://google-gson.googlecode.com/svn/trunk/gson/docs/javadocs/com/google/gson/stream/JsonReader.html#setLenient(boolean)
          jsonReader.setLenient(true);
          return gson.fromJson(jsonReader, returnType);
        } else {
          return gson.fromJson(respBody, returnType);
        }
      } catch (JsonSyntaxException e) {
        // for the String return type, return the response body string directly
        // if failed to deserialize JSON
        if (returnType.equals(String.class)) return (T) respBody;
        else throw(e);
      }
    } else {
      throw new ApiException(
        "Content type \"" + contentType + "\" is not supported",
        response.code(),
        response.headers().toMultimap(),
        respBody);
    }
  }

  /**
   * Serialize the given Java object into request body string, according to the
   * request Content-Type.
   *
   * @param obj The Java object
   * @param contentType The request Content-Type
   * @return The serialized string
   */
  public String serialize(Object obj, String contentType) throws ApiException {
    if (contentType.startsWith("application/json")) {
      if (obj != null)
        return gson.toJson(obj);
      else
        return null;
    } else {
      throw new ApiException("Content type \"" + contentType + "\" is not supported");
    }
  }

  /**
   * @see #execute(Call, Type)
   */
  public <T> T execute(Call call) throws ApiException {
    return execute(call, null);
  }

  /**
   * Execute HTTP call and deserialize the HTTP response body into the given return type.
   *
   * @param returnType The return type used to deserialize HTTP response body
   * @param <T> The return type corresponding to (same with) returnType
   * @return The Java object deserialized from response body. Returns null if returnType is null.
   */
  public <T> T execute(Call call, Type returnType) throws ApiException {
    try {
      Response response = call.execute();
      return handleResponse(response, returnType);
    } catch (IOException e) {
      throw new ApiException(e);
    }
  }

  /**
   * #see executeAsync(Call, Type, ApiCallback)
   */
  public <T> void executeAsync(Call call, ApiCallback<T> callback) throws ApiException {
    executeAsync(call, null, callback);
  }

  /**
   * Execute HTTP call asynchronously.
   *
   * @see #execute(Call, Type)
   * @param The callback to be executed when the API call finishes
   */
  public <T> void executeAsync(Call call, final Type returnType, final ApiCallback<T> callback) {
    call.enqueue(new Callback() {
      @Override
      public void onFailure(Request request, IOException e) {
        callback.onFailure(new ApiException(e));
      }

      @Override
      public void onResponse(Response response) throws IOException {
        T result;
        try {
          result = (T) handleResponse(response, returnType);
        } catch (ApiException e) {
          callback.onFailure(e);
          return;
        }
        callback.onSuccess(result);
      }
    });
  }

  public <T> T handleResponse(Response response, Type returnType) throws ApiException {
    if (response.isSuccessful()) {
      if (returnType == null || response.code() == 204) {
        // returning null if the returnType is not defined,
        // or the status code is 204 (No Content)
        return null;
      } else {
        return deserialize(response, returnType);
      }
    } else {
      String respBody = null;
      if (response.body() != null) {
        try {
          respBody = response.body().string();
        } catch (IOException e) {
          throw new ApiException(response.message(), e, response.code(), response.headers().toMultimap());
        }
      }
      throw new ApiException(response.message(), response.code(), response.headers().toMultimap(), respBody);
    }
  }

  /**
   * Build HTTP call with the given options.
   *
   * @param path The sub-path of the HTTP URL
   * @param method The request method, one of "GET", "HEAD", "POST", "PUT", "PATCH" and "DELETE"
   * @param queryParams The query parameters
   * @param body The request body object
   * @param headerParams The header parameters
   * @param formParams The form parameters
   * @param authNames The authentications to apply
   * @return The HTTP call
   */
  public Call buildCall(String path, String method, Map<String, Object> queryParams, Object body, Map<String, Object> headerParams, Map<String, Object> formParams, String[] authNames) throws ApiException {
    updateParamsForAuth(authNames, queryParams, headerParams);

    final String url = buildUrl(path, queryParams);
    final Request.Builder reqBuilder = new Request.Builder().url(url);
    processHeaderParams(headerParams, reqBuilder);

    String contentType = (String) headerParams.get("Content-Type");
    // ensuring a default content type
    if (contentType == null) contentType = "application/json";

    RequestBody reqBody;
    if ("GET".equals(method) || "HEAD".equals(method)) {
      reqBody = null;
    } else if ("application/x-www-form-urlencoded".equals(contentType)) {
      reqBody = buildRequestBodyFormEncoding(formParams);
    } else if ("multipart/form-data".equals(contentType)) {
      reqBody = buildRequestBodyMultipart(formParams);
    } else if (body == null) {
      if ("DELETE".equals(method)) {
        // allow calling DELETE without sending a request body
        reqBody = null;
      } else {
        // use an empty request body (for POST, PUT and PATCH)
        reqBody = RequestBody.create(MediaType.parse(contentType), "");
      }
    } else {
      reqBody = RequestBody.create(MediaType.parse(contentType), serialize(body, contentType));
    }

    Request request;
    if ("GET".equals(method)) {
      request = reqBuilder.get().build();
    } else if ("HEAD".equals(method)) {
      request = reqBuilder.head().build();
    } else if ("POST".equals(method)) {
      request = reqBuilder.post(reqBody).build();
    } else if ("PUT".equals(method)) {
      request = reqBuilder.put(reqBody).build();
    } else if ("PATCH".equals(method)) {
      request = reqBuilder.patch(reqBody).build();
    } else if ("DELETE".equals(method)) {
      if (reqBody == null) {
        // calling DELETE without sending a request body
        request = reqBuilder.delete().build();
      } else {
        request = reqBuilder.delete(reqBody).build();
      }
    } else {
      throw new ApiException("unknown method type: " + method);
    }

    return httpClient.newCall(request);
  }

  /**
   * Build full URL by concatenating base path, the given sub path and query parameters.
   *
   * @param path The sub path
   * @param queryParams The query parameters
   * @return The full URL
   */
  public String buildUrl(String path, Map<String, Object> queryParams) {
    StringBuilder query = new StringBuilder();
    for (Entry<String, Object> param : queryParams.entrySet()) {
      if (param.getValue() != null) {
        if (query.toString().length() == 0)
          query.append("?");
        else
          query.append("&");
        String value = parameterToString(param.getValue());
        query.append(escapeString(param.getKey())).append("=").append(escapeString(value));
      }
    }
    return basePath + path + query.toString();
  }

  /**
   * Set header parameters to the request builder, including default headers.
   */
  public void processHeaderParams(Map<String, Object> headerParams, Request.Builder reqBuilder) {
    for (Entry<String, Object> param : headerParams.entrySet()) {
      reqBuilder.header(param.getKey(), parameterToString(param.getValue()));
    }
    for (Entry<String, String> header : defaultHeaderMap.entrySet()) {
      if (!headerParams.containsKey(header.getKey())) {
        reqBuilder.header(header.getKey(), parameterToString(header.getValue()));
      }
    }
  }

  /**
   * Update query and header parameters based on authentication settings.
   *
   * @param authNames The authentications to apply
   */
  public void updateParamsForAuth(String[] authNames, Map<String, Object> queryParams, Map<String, Object> headerParams) {
    for (String authName : authNames) {
      Authentication auth = authentications.get(authName);
      if (auth == null) throw new RuntimeException("Authentication undefined: " + authName);
      auth.applyToParams(queryParams, headerParams);
    }
  }

  /**
   * Build a form-encoding request body with the given form parameters.
   */
  public RequestBody buildRequestBodyFormEncoding(Map<String, Object> formParams) {
    FormEncodingBuilder formBuilder  = new FormEncodingBuilder();
    for (Entry<String, Object> param : formParams.entrySet()) {
      formBuilder.add(param.getKey(), parameterToString(param.getValue()));
    }
    return formBuilder.build();
  }

  /**
   * Build a multipart (file uploading) request body with the given form parameters,
   * which could contain text fields and file fields.
   */
  public RequestBody buildRequestBodyMultipart(Map<String, Object> formParams) {
    MultipartBuilder mpBuilder = new MultipartBuilder().type(MultipartBuilder.FORM);
    for (Entry<String, Object> param : formParams.entrySet()) {
      if (param.getValue() instanceof File) {
        File file = (File) param.getValue();
        Headers partHeaders = Headers.of("Content-Disposition", "form-data; name=\"" + param.getKey() + "\"; filename=\"" + file.getName() + "\"");
        MediaType mediaType = MediaType.parse(guessContentTypeFromFile(file));
        mpBuilder.addPart(partHeaders, RequestBody.create(mediaType, file));
      } else {
        Headers partHeaders = Headers.of("Content-Disposition", "form-data; name=\"" + param.getKey() + "\"");
        mpBuilder.addPart(partHeaders, RequestBody.create(null, parameterToString(param.getValue())));
      }
    }
    return mpBuilder.build();
  }

  /**
   * Guess Content-Type header from the given file (defaults to "application/octet-stream").
   *
   * @param file The given file
   * @return The Content-Type guessed
   */
  public String guessContentTypeFromFile(File file) {
    String contentType = URLConnection.guessContentTypeFromName(file.getName());
    if (contentType == null) {
      return "application/octet-stream";
    } else {
      return contentType;
    }
  }
}
