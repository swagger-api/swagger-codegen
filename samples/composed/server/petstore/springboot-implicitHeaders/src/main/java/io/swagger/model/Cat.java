package io.swagger.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.model.Pet;
import java.util.List;
import org.springframework.validation.annotation.Validated;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * Cat
 */
@Validated
public class Cat extends Pet implements OneOfAllPetsResponseItems, OneOfPetPartItems, OneOfPup {
  @JsonProperty("hunts")
  private Boolean hunts = null;

  @JsonProperty("age")
  private Integer age = null;

  public Cat hunts(Boolean hunts) {
    this.hunts = hunts;
    return this;
  }

  /**
   * Get hunts
   * @return hunts
  **/
  @ApiModelProperty(value = "")
  
    public Boolean isHunts() {
    return hunts;
  }

  public void setHunts(Boolean hunts) {
    this.hunts = hunts;
  }

  public Cat age(Integer age) {
    this.age = age;
    return this;
  }

  /**
   * Get age
   * @return age
  **/
  @ApiModelProperty(value = "")
  
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
    return Objects.equals(this.hunts, cat.hunts) &&
        Objects.equals(this.age, cat.age) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(hunts, age, super.hashCode());
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
