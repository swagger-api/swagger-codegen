package io.swagger.model;

import io.swagger.model.Pet;
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

public class Cat extends Pet implements OneOfPup, OneOfAllPetsResponseItems  {
  
  @Schema(description = "")
  private Boolean hunts = null;
  
  @Schema(description = "")
  private Integer age = null;
 /**
   * Get hunts
   * @return hunts
  **/
  @JsonProperty("hunts")
  public Boolean isHunts() {
    return hunts;
  }

  public void setHunts(Boolean hunts) {
    this.hunts = hunts;
  }

  public Cat hunts(Boolean hunts) {
    this.hunts = hunts;
    return this;
  }

 /**
   * Get age
   * @return age
  **/
  @JsonProperty("age")
  public Integer getAge() {
    return age;
  }

  public void setAge(Integer age) {
    this.age = age;
  }

  public Cat age(Integer age) {
    this.age = age;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Cat {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    hunts: ").append(toIndentedString(hunts)).append("\n");
    sb.append("    age: ").append(toIndentedString(age)).append("\n");
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
