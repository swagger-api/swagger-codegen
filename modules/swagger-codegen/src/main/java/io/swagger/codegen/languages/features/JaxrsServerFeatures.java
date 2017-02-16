package io.swagger.codegen.languages.features;

/**
 * Features supported by CXF 3 server
 *
 */
public interface JaxrsServerFeatures
{

    public static final String USE_GENERIC_WS_RESPONSE = "useGenericWsResponse";

    public void setUseGenericWsResponse(boolean useGenericWsResponse);

}
