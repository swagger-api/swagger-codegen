package io.swagger.model;

import io.swagger.model.OuterEnum;



import io.swagger.annotations.*;
import java.util.Objects;


public class EnumTest   {
  

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

@XmlType(name="EnumTest")
@XmlEnum
public enum EnumTest {
    {values&#x3D;[UPPER, lower], enumVars&#x3D;[{name&#x3D;UPPER, value&#x3D;&quot;UPPER&quot;}, {name&#x3D;LOWER, value&#x3D;&quot;lower&quot;}]}, 
    
    public String value() {
        return name();
    }

    public static EnumTest fromValue(String v) {
        return valueOf(v);
    }
}
  private EnumStringEnum enumString = null;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

@XmlType(name="EnumTest")
@XmlEnum
public enum EnumTest {
    {values&#x3D;[1, -1], enumVars&#x3D;[{name&#x3D;NUMBER_1, value&#x3D;1}, {name&#x3D;NUMBER_MINUS_1, value&#x3D;-1}]}, 
    
    public String value() {
        return name();
    }

    public static EnumTest fromValue(String v) {
        return valueOf(v);
    }
}
  private EnumIntegerEnum enumInteger = null;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

@XmlType(name="EnumTest")
@XmlEnum
public enum EnumTest {
    {values&#x3D;[1.1, -1.2], enumVars&#x3D;[{name&#x3D;NUMBER_1_DOT_1, value&#x3D;1.1}, {name&#x3D;NUMBER_MINUS_1_DOT_2, value&#x3D;-1.2}]}, 
    
    public String value() {
        return name();
    }

    public static EnumTest fromValue(String v) {
        return valueOf(v);
    }
}
  private EnumNumberEnum enumNumber = null;
  private OuterEnum outerEnum = null;

  /**
   **/
  public EnumTest enumString(EnumStringEnum enumString) {
    this.enumString = enumString;
    return this;
  }

  
  @ApiModelProperty(example = "null", value = "")
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
  public EnumNumberEnum getEnumNumber() {
    return enumNumber;
  }
  public void setEnumNumber(EnumNumberEnum enumNumber) {
    this.enumNumber = enumNumber;
  }

  /**
   **/
  public EnumTest outerEnum(OuterEnum outerEnum) {
    this.outerEnum = outerEnum;
    return this;
  }

  
  @ApiModelProperty(example = "null", value = "")
  public OuterEnum getOuterEnum() {
    return outerEnum;
  }
  public void setOuterEnum(OuterEnum outerEnum) {
    this.outerEnum = outerEnum;
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
        Objects.equals(enumNumber, enumTest.enumNumber) &&
        Objects.equals(outerEnum, enumTest.outerEnum);
  }

  @Override
  public int hashCode() {
    return Objects.hash(enumString, enumInteger, enumNumber, outerEnum);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EnumTest {\n");
    
    sb.append("    enumString: ").append(toIndentedString(enumString)).append("\n");
    sb.append("    enumInteger: ").append(toIndentedString(enumInteger)).append("\n");
    sb.append("    enumNumber: ").append(toIndentedString(enumNumber)).append("\n");
    sb.append("    outerEnum: ").append(toIndentedString(outerEnum)).append("\n");
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
