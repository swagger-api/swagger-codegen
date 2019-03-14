package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.ScalaPlayFrameworkServerCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class ScalaPlayFrameworkServerCodegenOptionsProvider implements OptionsProvider {
    public static final String PROJECT_NAME_VALUE = "Swagger";

    @Override
    public String getLanguage() {
        return "scala-play-framework";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                //.put(ScalaPlayFrameworkServerCodegen.PROJECT_NAME, PROJECT_NAME_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}

