package io.swagger.model;

import java.util.HashMap;
import java.util.List;
import java.util.Map;



import io.swagger.annotations.*;
import java.util.Objects;


public class MapTest   {
  
  private Map<String, Map<String, String>> mapMapOfString = new HashMap<String, Map<String, String>>();

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

@XmlType(name="MapTest")
@XmlEnum
public enum MapTest {
    {values&#x3D;[UPPER, lower], enumVars&#x3D;[{name&#x3D;UPPER, value&#x3D;&quot;UPPER&quot;}, {name&#x3D;LOWER, value&#x3D;&quot;lower&quot;}]}, 
    
    public String value() {
        return name();
    }

    public static MapTest fromValue(String v) {
        return valueOf(v);
    }
}

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

@XmlType(name="MapTest")
@XmlEnum
public enum MapTest {
    {values&#x3D;[UPPER, lower], enumVars&#x3D;[{name&#x3D;UPPER, value&#x3D;&quot;UPPER&quot;}, {name&#x3D;LOWER, value&#x3D;&quot;lower&quot;}]}, 
    
    public String value() {
        return name();
    }

    public static MapTest fromValue(String v) {
        return valueOf(v);
    }
}
  private Map<String, InnerEnum> mapOfEnumString = new HashMap<String, InnerEnum>();

  /**
   **/
  public MapTest mapMapOfString(Map<String, Map<String, String>> mapMapOfString) {
    this.mapMapOfString = mapMapOfString;
    return this;
  }

  
  @ApiModelProperty(example = "null", value = "")
  public Map<String, Map<String, String>> getMapMapOfString() {
    return mapMapOfString;
  }
  public void setMapMapOfString(Map<String, Map<String, String>> mapMapOfString) {
    this.mapMapOfString = mapMapOfString;
  }

  /**
   **/
  public MapTest mapOfEnumString(Map<String, InnerEnum> mapOfEnumString) {
    this.mapOfEnumString = mapOfEnumString;
    return this;
  }

  
  @ApiModelProperty(example = "null", value = "")
  public Map<String, InnerEnum> getMapOfEnumString() {
    return mapOfEnumString;
  }
  public void setMapOfEnumString(Map<String, InnerEnum> mapOfEnumString) {
    this.mapOfEnumString = mapOfEnumString;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    MapTest mapTest = (MapTest) o;
    return Objects.equals(mapMapOfString, mapTest.mapMapOfString) &&
        Objects.equals(mapOfEnumString, mapTest.mapOfEnumString);
  }

  @Override
  public int hashCode() {
    return Objects.hash(mapMapOfString, mapOfEnumString);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class MapTest {\n");
    
    sb.append("    mapMapOfString: ").append(toIndentedString(mapMapOfString)).append("\n");
    sb.append("    mapOfEnumString: ").append(toIndentedString(mapOfEnumString)).append("\n");
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
