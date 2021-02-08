package io.swagger.model;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.v3.oas.annotations.media.Schema;




public class CatAllOf2   {
  @JsonProperty("hunts")
  private Boolean hunts = null;
  @JsonProperty("age")
  private Integer age = null;
  /**
   **/
  public CatAllOf2 hunts(Boolean hunts) {
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
  public CatAllOf2 age(Integer age) {
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
    CatAllOf2 catAllOf2 = (CatAllOf2) o;
    return Objects.equals(hunts, catAllOf2.hunts) &&
        Objects.equals(age, catAllOf2.age);
  }

  @Override
  public int hashCode() {
    return Objects.hash(hunts, age);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CatAllOf2 {\n");
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
