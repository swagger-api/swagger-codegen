package io.swagger.model;


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

public class PartFour  implements OneOfPartMasterOrigin  {
  
  @Schema(description = "")
  private String otherIdPart = null;
 /**
   * Get otherIdPart
   * @return otherIdPart
  **/
  @JsonProperty("otherIdPart")
  public String getOtherIdPart() {
    return otherIdPart;
  }

  public void setOtherIdPart(String otherIdPart) {
    this.otherIdPart = otherIdPart;
  }

  public PartFour otherIdPart(String otherIdPart) {
    this.otherIdPart = otherIdPart;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class PartFour {\n");
    
    sb.append("    otherIdPart: ").append(toIndentedString(otherIdPart)).append("\n");
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
