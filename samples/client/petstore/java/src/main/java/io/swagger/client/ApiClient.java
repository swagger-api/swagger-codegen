package io.swagger.client;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonReader;

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

public class ApiClient {
  private Map<String, String> defaultHeaderMap = new HashMap<String, String>();
  private boolean isDebug = false;
  private boolean lenientOnJson = false;
  private String basePath = "http://petstore.swagger.io/v2";

  private DateFormat dateFormat;
  private DateFormat datetimeFormat;

  private OkHttpClient httpClient;
  private Gson gson;

  public ApiClient() {
    // Use ISO 8601 format for date and datetime.
    // See https://en.wikipedia.org/wiki/ISO_8601
    dateFormat = new SimpleDateFormat("yyyy-MM-dd");
    datetimeFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");

    // Use UTC as the default time zone.
    dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    datetimeFormat.setTimeZone(TimeZone.getTimeZone("UTC"));

    // Set default User-Agent.
    setUserAgent("Java-Swagger");

    httpClient = new OkHttpClient();

    gson = new GsonBuilder()
      .serializeNulls()
      .setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
      .create();
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

  public ApiClient setUserAgent(String userAgent) {
    addDefaultHeader("User-Agent", userAgent);
    return this;
  }

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

  public boolean isDebug() {
    return isDebug;
  }

  public ApiClient enableDebug() {
    isDebug = true;
    return this;
  }

  public Date parseDateTime(String str) {
    try {
      return datetimeFormat.parse(str);
    } catch (java.text.ParseException e) {
      throw new RuntimeException(e);
    }
  }

  public Date parseDate(String str) {
    try {
      return dateFormat.parse(str);
    } catch (java.text.ParseException e) {
      throw new RuntimeException(e);
    }
  }

  public String formatDateTime(Date datetime) {
    return datetimeFormat.format(datetime);
  }

  public String formatDate(Date date) {
    return dateFormat.format(date);
  }

  public String parameterToString(Object param) {
    if (param == null) {
      return "";
    } else if (param instanceof Date) {
      return formatDateTime((Date) param);
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

  public String escapeString(String str) {
    try {
      return URLEncoder.encode(str, "utf8").replaceAll("\\+", "%20");
    } catch (UnsupportedEncodingException e) {
      return str;
    }
  }

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
      throw new ApiException(500, e.getMessage());
    }

    if (respBody == null || "".equals(respBody))
      return null;

    String contentType = response.headers().get("Content-Type");
    if (contentType == null) {
      // ensuring a default content type
      contentType = "application/json";
    }
    if (contentType.startsWith("application/json"))
      if (lenientOnJson) {
        JsonReader jsonReader = new JsonReader(new StringReader(respBody));
        // see https://google-gson.googlecode.com/svn/trunk/gson/docs/javadocs/com/google/gson/stream/JsonReader.html#setLenient(boolean)
        jsonReader.setLenient(true);
        return gson.fromJson(jsonReader, returnType);
      } else {
        return gson.fromJson(respBody, returnType);
      }
    else
      throw new ApiException(500, "Content type \"" + contentType + "\" is not supported");
  }

  public String serialize(Object obj, String contentType) throws ApiException {
    if (contentType.startsWith("application/json")) {
      if (obj != null)
        return gson.toJson(obj);
      else
        return null;
    } else {
      throw new ApiException(500, "Content type \"" + contentType + "\" is not supported");
    }
  }

  public <T> T invokeAPI(String path, String method, Map<String, Object> queryParams, Object body, Map<String, Object> headerParams, Map<String, Object> formParams, String contentType, Type returnType) throws ApiException {
    final String url = buildUrl(path, queryParams);
    Request.Builder reqBuilder = new Request.Builder().url(url);
    processHeaderParams(headerParams, reqBuilder);

    if (contentType == null) {
      // ensuring a default content type
      contentType = "application/json";
    }

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
      throw new ApiException(500, "unknown method type " + method);
    }

    try {
      Response response = httpClient.newCall(request).execute();
      if (response.isSuccessful()) {
        if (returnType == null || response.code() == 204) {
          // returning null if the returnType is not defined,
          // or the status code is 204 (No Content)
          return null;
        } else {
          return deserialize(response, returnType);
        }
      } else {
        String message = null;
        if (response.body() != null)
          message = response.body().string();
        if (message == null)
          message = "error";
        throw new ApiException(response.code(), message);
      }
    } catch (IOException e) {
      throw new ApiException(500, e.getMessage());
    }
  }

  /**
   * Build full URL by concatenating base path, the given sub path and query parameters.
   *
   * @param path The sub path
   * @param queryParams The query parameters
   * @return The full URL
   */
  protected String buildUrl(String path, Map<String, Object> queryParams) {
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
  protected void processHeaderParams(Map<String, Object> headerParams, Request.Builder reqBuilder) {
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
   * Build a form-encoding request body with the given form parameters.
   */
  protected RequestBody buildRequestBodyFormEncoding(Map<String, Object> formParams) {
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
  protected RequestBody buildRequestBodyMultipart(Map<String, Object> formParams) {
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
  protected String guessContentTypeFromFile(File file) {
    String contentType = URLConnection.guessContentTypeFromName(file.getName());
    if (contentType == null) {
      return "application/octet-stream";
    } else {
      return contentType;
    }
  }
}
