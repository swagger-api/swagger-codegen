package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.languages.JavaSparkjavaServerCodegen;

import java.util.Map;

public class JavaSparkjavaServerCodegenOptionsProvider implements OptionsProvider {
    public static final String PROJECT_NAME_VALUE = "Swagger";

    @Override
    public String getLanguage() {
        return "javaSparkjava";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(JavaSparkjavaServerCodegen.PROJECT_NAME, PROJECT_NAME_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}

