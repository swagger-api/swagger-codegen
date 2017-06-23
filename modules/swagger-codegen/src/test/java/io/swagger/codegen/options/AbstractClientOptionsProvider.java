package io.swagger.codegen.options;

import java.util.HashMap;
import java.util.Map;

import io.swagger.codegen.CodegenConstants;

public abstract class AbstractClientOptionsProvider implements OptionsProvider {

    public static final String MODEL_PROPERTY_NAMING_VALUE = "camelCase";
    public static final String MODEL_GETTERSETTER_NAMING_VALUE = "PascalCase";

  @Override
  public String getLanguage() {
    return null;
  }

  @Override
  public Map<String, String> createOptions() {
    Map<String, String> result = new HashMap<>();
    result.put(CodegenConstants.MODEL_PROPERTY_NAMING, MODEL_PROPERTY_NAMING_VALUE);
    result.put(CodegenConstants.MODEL_GETTERSETTER_NAMING, MODEL_GETTERSETTER_NAMING_VALUE);
    return result;
  }

  @Override
  public boolean isServer() {
    return false;
  }

}
