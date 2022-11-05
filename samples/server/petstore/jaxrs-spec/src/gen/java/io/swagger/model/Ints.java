package io.swagger.model;

import io.swagger.annotations.ApiModel;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;


/**
 * True or False indicator
 **/
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * True or False indicator
 */
public enum Ints {
  
  NUMBER_0(0),
  
  NUMBER_1(1),
  
  NUMBER_2(2),
  
  NUMBER_3(3),
  
  NUMBER_4(4),
  
  NUMBER_5(5),
  
  NUMBER_6(6);

  private Integer value;

  Ints(Integer value) {
    this.value = value;
  }

  @Override
  @JsonValue
  public String toString() {
    return String.valueOf(value);
  }

  @JsonCreator
  public static Ints fromValue(String text) {
    for (Ints b : Ints.values()) {
      if (String.valueOf(b.value).equals(text)) {
        return b;
      }
    }
    return null;
  }
}


