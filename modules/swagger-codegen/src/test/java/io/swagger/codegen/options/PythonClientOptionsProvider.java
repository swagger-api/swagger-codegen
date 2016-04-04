package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class PythonClientOptionsProvider implements OptionsProvider {
    public static final String PACKAGE_NAME_VALUE = "swagger_client_python";
    public static final String PACKAGE_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String PACKAGE_TITLE_VALUE = "Swagger client library";
    public static final String PACKAGE_AUTHOR_VALUE = "Swagger";
    public static final String PACKAGE_AUTHOR_EMAIL_VALUE = "";
    public static final String PACKAGE_URL_VALUE = "";
    public static final String PACKAGE_KEYWORDS_VALUE = "";
    public static final String PACKAGE_INSTALL_REQUIRES_VALUE = "";
    public static final String PACKAGE_DESCRIPTION_VALUE = "A swagger client library";

    @Override
    public String getLanguage() {
        return "python";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.PACKAGE_TITLE, PACKAGE_TITLE_VALUE)
                .put(CodegenConstants.PACKAGE_AUTHOR, PACKAGE_AUTHOR_VALUE)
                .put(CodegenConstants.PACKAGE_AUTHOR_EMAIL, PACKAGE_AUTHOR_EMAIL_VALUE)
                .put(CodegenConstants.PACKAGE_URL, PACKAGE_URL_VALUE)
                .put(CodegenConstants.PACKAGE_KEYWORDS, PACKAGE_KEYWORDS_VALUE)
                .put(CodegenConstants.PACKAGE_INSTALL_REQUIRES, PACKAGE_INSTALL_REQUIRES_VALUE)
                .put(CodegenConstants.PACKAGE_DESCRIPTION, PACKAGE_DESCRIPTION_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "true")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
