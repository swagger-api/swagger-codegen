package io.swagger.model;

import java.util.ArrayList;
import java.util.List;



import io.swagger.annotations.*;
import java.util.Objects;


public class EnumArrays   {
  

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

@XmlType(name="EnumArrays")
@XmlEnum
public enum EnumArrays {
    {values&#x3D;[&gt;&#x3D;, $], enumVars&#x3D;[{name&#x3D;GREATER_THAN_OR_EQUAL_TO, value&#x3D;&quot;&gt;&#x3D;&quot;}, {name&#x3D;DOLLAR, value&#x3D;&quot;$&quot;}]}, 
    
    public String value() {
        return name();
    }

    public static EnumArrays fromValue(String v) {
        return valueOf(v);
    }
}
  private JustSymbolEnum justSymbol = null;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

@XmlType(name="EnumArrays")
@XmlEnum
public enum EnumArrays {
    {values&#x3D;[fish, crab], enumVars&#x3D;[{name&#x3D;FISH, value&#x3D;&quot;fish&quot;}, {name&#x3D;CRAB, value&#x3D;&quot;crab&quot;}]}, 
    
    public String value() {
        return name();
    }

    public static EnumArrays fromValue(String v) {
        return valueOf(v);
    }
}

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

@XmlType(name="EnumArrays")
@XmlEnum
public enum EnumArrays {
    {values&#x3D;[fish, crab], enumVars&#x3D;[{name&#x3D;FISH, value&#x3D;&quot;fish&quot;}, {name&#x3D;CRAB, value&#x3D;&quot;crab&quot;}]}, 
    
    public String value() {
        return name();
    }

    public static EnumArrays fromValue(String v) {
        return valueOf(v);
    }
}
  private List<ArrayEnumEnum> arrayEnum = new ArrayList<ArrayEnumEnum>();

  /**
   **/
  public EnumArrays justSymbol(JustSymbolEnum justSymbol) {
    this.justSymbol = justSymbol;
    return this;
  }

  
  @ApiModelProperty(example = "null", value = "")
  public JustSymbolEnum getJustSymbol() {
    return justSymbol;
  }
  public void setJustSymbol(JustSymbolEnum justSymbol) {
    this.justSymbol = justSymbol;
  }

  /**
   **/
  public EnumArrays arrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
    return this;
  }

  
  @ApiModelProperty(example = "null", value = "")
  public List<ArrayEnumEnum> getArrayEnum() {
    return arrayEnum;
  }
  public void setArrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    EnumArrays enumArrays = (EnumArrays) o;
    return Objects.equals(justSymbol, enumArrays.justSymbol) &&
        Objects.equals(arrayEnum, enumArrays.arrayEnum);
  }

  @Override
  public int hashCode() {
    return Objects.hash(justSymbol, arrayEnum);
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}
