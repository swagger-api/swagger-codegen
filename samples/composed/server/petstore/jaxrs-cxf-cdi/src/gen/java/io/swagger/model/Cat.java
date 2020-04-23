package io.swagger.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.model.Pet;
import java.util.List;
import javax.validation.constraints.*;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.Objects;

import javax.xml.bind.annotation.*;



public class Cat extends Pet implements OneOfAllPetsResponseItems, OneOfPetPartItems, OneOfPup  {
  private Boolean hunts = null;
  private Integer age = null;

  /**
   **/
  public Cat hunts(Boolean hunts) {
    this.hunts = hunts;
    return this;
  }

  
  
  @Schema(description = "")
  @JsonProperty("hunts")
  public Boolean isHunts() {
    return hunts;
  }
  public void setHunts(Boolean hunts) {
    this.hunts = hunts;
  }

  /**
   **/
  public Cat age(Integer age) {
    this.age = age;
    return this;
  }

  
  
  @Schema(description = "")
  @JsonProperty("age")
  public Integer getAge() {
    return age;
  }
  public void setAge(Integer age) {
    this.age = age;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Cat cat = (Cat) o;
    return Objects.equals(hunts, cat.hunts) &&
        Objects.equals(age, cat.age);
  }

  @Override
  public int hashCode() {
    return Objects.hash(hunts, age);
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}
