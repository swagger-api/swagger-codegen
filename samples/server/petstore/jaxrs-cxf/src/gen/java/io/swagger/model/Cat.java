package io.swagger.model;

import io.swagger.model.Animal;


import javax.validation.constraints.*;






import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonProperty;



public class Cat extends Animal {
  

  @ApiModelProperty(required = true, value = "")



  private String className = null;

  

  @ApiModelProperty(value = "")



  private String color = "red";

  

  @ApiModelProperty(value = "")



  private Boolean declawed = null;

  
  
 /**
  
  
   * Get className
  
  
  
   * @return className
  **/
  @JsonProperty("className")


  @NotNull

  public String getClassName() {
    return className;
  }

  
  public void setClassName(String className) {
    this.className = className;
  }

  public Cat className(String className) {
    this.className = className;
    return this;
  }
  
  
  

  
 /**
  
  
   * Get color
  
  
  
   * @return color
  **/
  @JsonProperty("color")


  public String getColor() {
    return color;
  }

  
  public void setColor(String color) {
    this.color = color;
  }

  public Cat color(String color) {
    this.color = color;
    return this;
  }
  
  
  

  
 /**
  
  
   * Get declawed
  
  
  
   * @return declawed
  **/
  @JsonProperty("declawed")


  public Boolean isisDeclawed() {
    return declawed;
  }

  
  public void setDeclawed(Boolean declawed) {
    this.declawed = declawed;
  }

  public Cat declawed(Boolean declawed) {
    this.declawed = declawed;
    return this;
  }
  
  
  

  

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Cat {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    className: ").append(toIndentedString(className)).append("\n");
    sb.append("    color: ").append(toIndentedString(color)).append("\n");
    sb.append("    declawed: ").append(toIndentedString(declawed)).append("\n");
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




