package io.swagger.model;

import io.swagger.annotations.ApiModel;
import java.math.BigDecimal;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;


/**
 * some number
 **/
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * some number
 */
public enum Numbers {
  
  NUMBER_7(new BigDecimal(7)),
  
  NUMBER_8(new BigDecimal(8)),
  
  NUMBER_9(new BigDecimal(9)),
  
  NUMBER_10(new BigDecimal(10));

  private BigDecimal value;

  Numbers(BigDecimal value) {
    this.value = value;
  }

  @Override
  @JsonValue
  public String toString() {
    return String.valueOf(value);
  }

  @JsonCreator
  public static Numbers fromValue(String text) {
    for (Numbers b : Numbers.values()) {
      if (String.valueOf(b.value).equals(text)) {
        return b;
      }
    }
    return null;
  }
}


