package io.swagger.model;

import io.swagger.annotations.ApiModel;
import javax.validation.constraints.*;
import javax.validation.Valid;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * True or False indicator
 */
public enum ModelBoolean {
  
  TRUE(true),
  
  FALSE(false);

  private Boolean value;

  ModelBoolean(Boolean value) {
    this.value = value;
  }

  @Override
  @JsonValue
  public String toString() {
    return String.valueOf(value);
  }

  @JsonCreator
  public static ModelBoolean fromValue(String text) {
    for (ModelBoolean b : ModelBoolean.values()) {
      if (String.valueOf(b.value).equals(text)) {
        return b;
      }
    }
    return null;
  }
  
}

