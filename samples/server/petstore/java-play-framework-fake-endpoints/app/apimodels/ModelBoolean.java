package apimodels;

import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import com.fasterxml.jackson.annotation.JsonCreator;

/**
 * True or False indicator
 */
public enum ModelBoolean {
  
  TRUE(true),
  
  FALSE(false);

  private final Boolean value;

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

