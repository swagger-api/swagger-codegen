package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.languages.SwaggerYamlGenerator;

import java.util.Map;

public class SwaggerYamlOptionsProvider implements OptionsProvider {
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String OUTPUT_NAME = "swagger.yaml";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String MINIMIZE_QUOTES = "true";

    @Override
    public String getLanguage() {
        return "swagger-yaml";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(SwaggerYamlGenerator.OUTPUT_NAME, OUTPUT_NAME)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(SwaggerYamlGenerator.MINMIZE_QUOTES, MINIMIZE_QUOTES)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
