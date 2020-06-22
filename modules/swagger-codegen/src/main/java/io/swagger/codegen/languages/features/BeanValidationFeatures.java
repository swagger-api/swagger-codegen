package io.swagger.codegen.languages.features;

public interface BeanValidationFeatures {

    // Language supports generating BeanValidation-Annotations
    String USE_BEANVALIDATION = "useBeanValidation";

    void setUseBeanValidation(boolean useBeanValidation);
    
}
