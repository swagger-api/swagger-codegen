package io.swagger.codegen.options;

import java.util.Map;

public interface OptionsProvider {
    public static final String SUPPORTS_MODEL_EXTENSION_VALUE = "false";
    String getLanguage();
    Map<String, String> createOptions();
    boolean isServer();
}
