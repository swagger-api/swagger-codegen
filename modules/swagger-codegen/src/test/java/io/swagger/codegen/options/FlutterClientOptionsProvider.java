package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.FlutterClientCodegen;

import java.util.Map;

public class FlutterClientOptionsProvider implements OptionsProvider {
    public static final String SORT_PARAMS_VALUE = "true";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String PUB_NAME_VALUE = "swagger";
    public static final String PUB_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String PUB_DESCRIPTION_VALUE = "Swagger API client flytter";
    public static final String SOURCE_FOLDER_VALUE = "src";
    public static final String USE_ENUM_EXTENSION = "true";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";


    @Override
    public String getLanguage() {
        return "flutter";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(FlutterClientCodegen.PUB_NAME, PUB_NAME_VALUE)
                .put(FlutterClientCodegen.PUB_VERSION, PUB_VERSION_VALUE)
                .put(FlutterClientCodegen.PUB_DESCRIPTION, PUB_DESCRIPTION_VALUE)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(FlutterClientCodegen.USE_ENUM_EXTENSION, USE_ENUM_EXTENSION)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
