package io.swagger.model;

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

public class Macaw  implements AnyOfbody1ParrotsItems, AnyOfbody2, OneOfinlineResponse200ParrotsItems, OneOfinlineResponse2001  {
  
  @Schema(description = "")
  private String color = null;
  
  @Schema(description = "")
  private Boolean singer = null;
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

  public Macaw color(String color) {
    this.color = color;
    return this;
  }

 /**
   * Get singer
   * @return singer
  **/
  @JsonProperty("singer")
  public Boolean isSinger() {
    return singer;
  }

  public void setSinger(Boolean singer) {
    this.singer = singer;
  }

  public Macaw singer(Boolean singer) {
    this.singer = singer;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Macaw {\n");
    
    sb.append("    color: ").append(toIndentedString(color)).append("\n");
    sb.append("    singer: ").append(toIndentedString(singer)).append("\n");
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
