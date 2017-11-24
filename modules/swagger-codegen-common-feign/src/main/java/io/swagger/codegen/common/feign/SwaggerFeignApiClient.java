package io.swagger.codegen.common.feign;

import feign.Feign;
import feign.RequestInterceptor;

import java.util.Map;


public interface SwaggerFeignApiClient {

    String getBasePath();

    SwaggerFeignApiClient setBasePath(String basePath);

    Map<String, RequestInterceptor> getApiAuthorizations();

    void setApiAuthorizations(Map<String, RequestInterceptor> apiAuthorizations);

    Feign.Builder getFeignBuilder();

    SwaggerFeignApiClient setFeignBuilder(Feign.Builder feignBuilder);

    void setApiKey(String apiKey);

    void setCredentials(String username, String password);

    void setAccessToken(String accessToken, Long expiresIn);

    void configureAuthorizationFlow(String clientId, String clientSecret, String redirectURI);

    RequestInterceptor getAuthorization(String authName);

    void addAuthorization(String authName, RequestInterceptor authorization);
}
