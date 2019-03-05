package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.GoClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class GoClientOptionsProvider implements OptionsProvider {

    public static final String PACKAGE_VERSION_VALUE = "1.0.0";
    public static final String PACKAGE_NAME_VALUE = "Go";
    public static final boolean WITH_XML_VALUE = true;
    public static final String API_PATH = "go";
    public static final String API_VERSION = "0.0.1";
    public static final String GO_MODULE = "github.com/foo/bar";
    public static final String SERVER_PORT = "8080";

    @Override
    public String getLanguage() {
        return "go";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .put(CodegenConstants.WITH_XML, "true")
                .put(CodegenConstants.API_PATH, API_PATH)
                .put(CodegenConstants.API_VERSION, API_VERSION)
                .put(CodegenConstants.GO_MODULE, GO_MODULE)
                .put(CodegenConstants.SERVER_PORT, SERVER_PORT)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
