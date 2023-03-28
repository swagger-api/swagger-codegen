package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;

import com.google.common.collect.ImmutableMap;

import java.util.HashMap;
import java.util.Map;

public class ScalaClientOptionsProvider extends LagomScalaOptionsProvider {
    public static final String INVOKER_PACKAGE_VALUE = "io.swagger.client.test";


    @Override
    public String getLanguage() {
        return "scala";
    }

    @Override
    public Map<String, String> createOptions() {
        Map<String, String> options = new HashMap(super.createOptions());
        options.put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE);
        return options;
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
