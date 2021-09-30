package io.swagger.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;
import org.springframework.validation.annotation.Validated;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * EnumFormBody
 */
@Validated


public class EnumFormBody   {
  /**
   * Gets or Sets enumFormStringArray
   */
  public enum EnumFormStringArrayEnum {
    GREATER_THAN(">"),
    
    DOLLAR("$");

    private String value;

    EnumFormStringArrayEnum(String value) {
      this.value = value;
    }

    @Override
    @JsonValue
    public String toString() {
      return String.valueOf(value);
    }

    @JsonCreator
    public static EnumFormStringArrayEnum fromValue(String text) {
      for (EnumFormStringArrayEnum b : EnumFormStringArrayEnum.values()) {
        if (String.valueOf(b.value).equals(text)) {
          return b;
        }
      }
      return null;
    }
  }
  @JsonProperty("enum_form_string_array")
  @Valid
  private List<EnumFormStringArrayEnum> enumFormStringArray = null;

  /**
   * Form parameter enum test (string)
   */
  public enum EnumFormStringEnum {
    _ABC("_abc"),
    
    _EFG("-efg"),
    
    _XYZ_("(xyz)");

    private String value;

    EnumFormStringEnum(String value) {
      this.value = value;
    }

    @Override
    @JsonValue
    public String toString() {
      return String.valueOf(value);
    }

    @JsonCreator
    public static EnumFormStringEnum fromValue(String text) {
      for (EnumFormStringEnum b : EnumFormStringEnum.values()) {
        if (String.valueOf(b.value).equals(text)) {
          return b;
        }
      }
      return null;
    }
  }
  @JsonProperty("enum_form_string")
  private EnumFormStringEnum enumFormString = EnumFormStringEnum._EFG;

  /**
   * Query parameter enum test (double)
   */
  public enum EnumQueryDoubleEnum {
    NUMBER_1_DOT_1(1.1),
    
    NUMBER_MINUS_1_DOT_2(-1.2);

    private Double value;

    EnumQueryDoubleEnum(Double value) {
      this.value = value;
    }

    @Override
    @JsonValue
    public String toString() {
      return String.valueOf(value);
    }

    @JsonCreator
    public static EnumQueryDoubleEnum fromValue(String text) {
      for (EnumQueryDoubleEnum b : EnumQueryDoubleEnum.values()) {
        if (String.valueOf(b.value).equals(text)) {
          return b;
        }
      }
      return null;
    }
  }
  @JsonProperty("enum_query_double")
  private EnumQueryDoubleEnum enumQueryDouble = null;

  public EnumFormBody enumFormStringArray(List<EnumFormStringArrayEnum> enumFormStringArray) {
    this.enumFormStringArray = enumFormStringArray;
    return this;
  }

  public EnumFormBody addEnumFormStringArrayItem(EnumFormStringArrayEnum enumFormStringArrayItem) {
    if (this.enumFormStringArray == null) {
      this.enumFormStringArray = new ArrayList<>();
    }
    this.enumFormStringArray.add(enumFormStringArrayItem);
    return this;
  }

  /**
   * Form parameter enum test (string array)
   * @return enumFormStringArray
   **/
  @Schema(description = "Form parameter enum test (string array)")
  
    public List<EnumFormStringArrayEnum> getEnumFormStringArray() {
    return enumFormStringArray;
  }

  public void setEnumFormStringArray(List<EnumFormStringArrayEnum> enumFormStringArray) {
    this.enumFormStringArray = enumFormStringArray;
  }

  public EnumFormBody enumFormString(EnumFormStringEnum enumFormString) {
    this.enumFormString = enumFormString;
    return this;
  }

  /**
   * Form parameter enum test (string)
   * @return enumFormString
   **/
  @Schema(description = "Form parameter enum test (string)")
  
    public EnumFormStringEnum getEnumFormString() {
    return enumFormString;
  }

  public void setEnumFormString(EnumFormStringEnum enumFormString) {
    this.enumFormString = enumFormString;
  }

  public EnumFormBody enumQueryDouble(EnumQueryDoubleEnum enumQueryDouble) {
    this.enumQueryDouble = enumQueryDouble;
    return this;
  }

  /**
   * Query parameter enum test (double)
   * @return enumQueryDouble
   **/
  @Schema(description = "Query parameter enum test (double)")
  
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
    EnumFormBody enumFormBody = (EnumFormBody) o;
    return Objects.equals(this.enumFormStringArray, enumFormBody.enumFormStringArray) &&
        Objects.equals(this.enumFormString, enumFormBody.enumFormString) &&
        Objects.equals(this.enumQueryDouble, enumFormBody.enumQueryDouble);
  }

  @Override
  public int hashCode() {
    return Objects.hash(enumFormStringArray, enumFormString, enumQueryDouble);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EnumFormBody {\n");
    
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
