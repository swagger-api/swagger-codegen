package io.swagger.model;

import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class Body4   {

  public enum EnumFormStringArrayEnum {
  
      GREATER_THAN(String.valueOf(">")), DOLLAR(String.valueOf("$"));
  
  
      private String value;
  
      EnumFormStringArrayEnum (String v) {
          value = v;
      }
  
      public String value() {
          return value;
      }
  
      @Override
      @JsonValue
      public String toString() {
          return String.valueOf(value);
      }
  
      @JsonCreator
      public static EnumFormStringArrayEnum fromValue(String v) {
          for (EnumFormStringArrayEnum b : EnumFormStringArrayEnum.values()) {
              if (String.valueOf(b.value).equals(v)) {
                  return b;
              }
          }
          return null;
      }
  }
  private @Valid List<EnumFormStringArrayEnum> enumFormStringArray = new ArrayList<EnumFormStringArrayEnum>();
public enum EnumFormStringEnum {

    _ABC(String.valueOf("_abc")), _EFG(String.valueOf("-efg")), _XYZ_(String.valueOf("(xyz)"));


    private String value;

    EnumFormStringEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static EnumFormStringEnum fromValue(String v) {
        for (EnumFormStringEnum b : EnumFormStringEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}
  private @Valid EnumFormStringEnum enumFormString = EnumFormStringEnum._EFG;
public enum EnumQueryDoubleEnum {

    NUMBER_1_DOT_1(Double.valueOf(1.1)), NUMBER_MINUS_1_DOT_2(Double.valueOf(-1.2));


    private Double value;

    EnumQueryDoubleEnum (Double v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static EnumQueryDoubleEnum fromValue(String v) {
        for (EnumQueryDoubleEnum b : EnumQueryDoubleEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}
  private @Valid EnumQueryDoubleEnum enumQueryDouble = null;

  /**
   * Form parameter enum test (string array)
   **/
  public Body4 enumFormStringArray(List<EnumFormStringArrayEnum> enumFormStringArray) {
    this.enumFormStringArray = enumFormStringArray;
    return this;
  }

  
  @ApiModelProperty(value = "Form parameter enum test (string array)")
  @JsonProperty("enum_form_string_array")

  public List<EnumFormStringArrayEnum> getEnumFormStringArray() {
    return enumFormStringArray;
  }
  public void setEnumFormStringArray(List<EnumFormStringArrayEnum> enumFormStringArray) {
    this.enumFormStringArray = enumFormStringArray;
  }

  /**
   * Form parameter enum test (string)
   **/
  public Body4 enumFormString(EnumFormStringEnum enumFormString) {
    this.enumFormString = enumFormString;
    return this;
  }

  
  @ApiModelProperty(value = "Form parameter enum test (string)")
  @JsonProperty("enum_form_string")

  public EnumFormStringEnum getEnumFormString() {
    return enumFormString;
  }
  public void setEnumFormString(EnumFormStringEnum enumFormString) {
    this.enumFormString = enumFormString;
  }

  /**
   * Query parameter enum test (double)
   **/
  public Body4 enumQueryDouble(EnumQueryDoubleEnum enumQueryDouble) {
    this.enumQueryDouble = enumQueryDouble;
    return this;
  }

  
  @ApiModelProperty(value = "Query parameter enum test (double)")
  @JsonProperty("enum_query_double")

  public EnumQueryDoubleEnum getEnumQueryDouble() {
    return enumQueryDouble;
  }
  public void setEnumQueryDouble(EnumQueryDoubleEnum enumQueryDouble) {
    this.enumQueryDouble = enumQueryDouble;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Body4 body4 = (Body4) o;
    return Objects.equals(enumFormStringArray, body4.enumFormStringArray) &&
        Objects.equals(enumFormString, body4.enumFormString) &&
        Objects.equals(enumQueryDouble, body4.enumQueryDouble);
  }

  @Override
  public int hashCode() {
    return Objects.hash(enumFormStringArray, enumFormString, enumQueryDouble);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Body4 {\n");
    
    sb.append("    enumFormStringArray: ").append(toIndentedString(enumFormStringArray)).append("\n");
    sb.append("    enumFormString: ").append(toIndentedString(enumFormString)).append("\n");
    sb.append("    enumQueryDouble: ").append(toIndentedString(enumQueryDouble)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}
