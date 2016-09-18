package io.swagger.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;





@javax.annotation.Generated(value = "class io.swagger.codegen.languages.UndertowCodegen", date = "2016-09-18T18:53:43.115-04:00")
public class EnumTest   {
  


  public enum EnumStringEnum {
    UPPER("UPPER"),
    LOWER("lower");

    private String value;

    EnumStringEnum(String value) {
      this.value = value;
    }

    @Override
    @JsonValue
    public String toString() {
      return value;
    }
  }

  private EnumStringEnum enumString = null;


  public enum EnumIntegerEnum {
    NUMBER_1(1),
    NUMBER_MINUS_1(-1);

    private String value;

    EnumIntegerEnum(String value) {
      this.value = value;
    }

    @Override
    @JsonValue
    public String toString() {
      return value;
    }
  }

  private EnumIntegerEnum enumInteger = null;


  public enum EnumNumberEnum {
    NUMBER_1_DOT_1(1.1),
    NUMBER_MINUS_1_DOT_2(-1.2);

    private String value;

    EnumNumberEnum(String value) {
      this.value = value;
    }

    @Override
    @JsonValue
    public String toString() {
      return value;
    }
  }

  private EnumNumberEnum enumNumber = null;

  /**
   **/
  public EnumTest enumString(EnumStringEnum enumString) {
    this.enumString = enumString;
    return this;
  }

  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("enum_string")
  public EnumStringEnum getEnumString() {
    return enumString;
  }
  public void setEnumString(EnumStringEnum enumString) {
    this.enumString = enumString;
  }

  /**
   **/
  public EnumTest enumInteger(EnumIntegerEnum enumInteger) {
    this.enumInteger = enumInteger;
    return this;
  }

  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("enum_integer")
  public EnumIntegerEnum getEnumInteger() {
    return enumInteger;
  }
  public void setEnumInteger(EnumIntegerEnum enumInteger) {
    this.enumInteger = enumInteger;
  }

  /**
   **/
  public EnumTest enumNumber(EnumNumberEnum enumNumber) {
    this.enumNumber = enumNumber;
    return this;
  }

  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("enum_number")
  public EnumNumberEnum getEnumNumber() {
    return enumNumber;
  }
  public void setEnumNumber(EnumNumberEnum enumNumber) {
    this.enumNumber = enumNumber;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    EnumTest enumTest = (EnumTest) o;
    return Objects.equals(enumString, enumTest.enumString) &&
        Objects.equals(enumInteger, enumTest.enumInteger) &&
        Objects.equals(enumNumber, enumTest.enumNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(enumString, enumInteger, enumNumber);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EnumTest {\n");
    
    sb.append("    enumString: ").append(toIndentedString(enumString)).append("\n");
    sb.append("    enumInteger: ").append(toIndentedString(enumInteger)).append("\n");
    sb.append("    enumNumber: ").append(toIndentedString(enumNumber)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

