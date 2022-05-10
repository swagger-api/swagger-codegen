package io.swagger.model;

import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;

import io.swagger.v3.oas.annotations.media.Schema;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonCreator;

public class EnumArrays   {
  public enum JustSymbolEnum {
    GREATER_THAN_OR_EQUAL_TO(">="),
    DOLLAR("$");

    private String value;

    JustSymbolEnum(String value) {
      this.value = value;
    }
    @JsonValue
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
    @JsonCreator
    public static JustSymbolEnum fromValue(String text) {
      for (JustSymbolEnum b : JustSymbolEnum.values()) {
        if (String.valueOf(b.value).equals(text)) {
          return b;
        }
      }
      return null;
    }
  }  
  @Schema(description = "")
  private JustSymbolEnum justSymbol = null;
  public enum ArrayEnumEnum {
    FISH("fish"),
    CRAB("crab");

    private String value;

    ArrayEnumEnum(String value) {
      this.value = value;
    }
    @JsonValue
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
    @JsonCreator
    public static ArrayEnumEnum fromValue(String text) {
      for (ArrayEnumEnum b : ArrayEnumEnum.values()) {
        if (String.valueOf(b.value).equals(text)) {
          return b;
        }
      }
      return null;
    }
  }  
  @Schema(description = "")
  private List<ArrayEnumEnum> arrayEnum = null;
 /**
   * Get justSymbol
   * @return justSymbol
  **/
  @JsonProperty("just_symbol")
  public String getJustSymbol() {
    if (justSymbol == null) {
      return null;
    }
    return justSymbol.getValue();
  }

  public void setJustSymbol(JustSymbolEnum justSymbol) {
    this.justSymbol = justSymbol;
  }

  public EnumArrays justSymbol(JustSymbolEnum justSymbol) {
    this.justSymbol = justSymbol;
    return this;
  }

 /**
   * Get arrayEnum
   * @return arrayEnum
  **/
  @JsonProperty("array_enum")
  public List<ArrayEnumEnum> getArrayEnum() {
    return arrayEnum;
  }

  public void setArrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
  }

  public EnumArrays arrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
    return this;
  }

  public EnumArrays addArrayEnumItem(ArrayEnumEnum arrayEnumItem) {
    this.arrayEnum.add(arrayEnumItem);
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EnumArrays {\n");
    
    sb.append("    justSymbol: ").append(toIndentedString(justSymbol)).append("\n");
    sb.append("    arrayEnum: ").append(toIndentedString(arrayEnum)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private static String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}
