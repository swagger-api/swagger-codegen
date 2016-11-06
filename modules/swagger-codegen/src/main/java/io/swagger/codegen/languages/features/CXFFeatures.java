package io.swagger.codegen.languages.features;

/**
 * Features supported by CXF 3 (client + server)
 *
 */
public interface CXFFeatures extends LoggingFeatures, GzipFeatures, BeanValidationFeatures, BeanValidationExtendedFeatures {

    public static final String USE_JAXB_ANNOTATIONS = "useJaxbAnnotations";

    public void setUseJaxbAnnotations(boolean useJaxbAnnotations);

}
