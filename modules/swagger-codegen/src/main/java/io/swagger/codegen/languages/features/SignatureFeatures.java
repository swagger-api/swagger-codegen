package io.swagger.codegen.languages.features;

public interface SignatureFeatures {

  public static final String METHODS_TO_HTTP_REQUEST_STRATEGY = "methodsPerHttpRequestStrategy";

  public void setMethodsPerHttpRequestStrategy(MethodsPerHttpRequestStrategy methodsPerHttpRequestStrategy);

  public static enum MethodsPerHttpRequestStrategy {

    ONE_PER_HTTP_REQUST_METHOD, 
    OVERLOADING;

    public static MethodsPerHttpRequestStrategy fromString(String value) {
      try {
        return MethodsPerHttpRequestStrategy.valueOf(value.toUpperCase());
      } catch (IllegalArgumentException | NullPointerException e) {
        return ONE_PER_HTTP_REQUST_METHOD;
      }
    }

  }

}
