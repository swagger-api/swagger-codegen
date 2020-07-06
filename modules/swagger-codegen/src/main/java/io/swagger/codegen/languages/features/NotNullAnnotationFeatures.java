package io.swagger.codegen.languages.features;

public interface NotNullAnnotationFeatures {
    // Language supports generating not Null Jackson Annotation
    String NOT_NULL_JACKSON_ANNOTATION = "notNullJacksonAnnotation";

    void setNotNullJacksonAnnotation(boolean notNullJacksonAnnotation);

    boolean isNotNullJacksonAnnotation();
}
