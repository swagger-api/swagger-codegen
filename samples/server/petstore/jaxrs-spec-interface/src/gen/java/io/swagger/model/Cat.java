package io.swagger.model;

import io.swagger.model.Animal;



import javax.validation.constraints.*;
import javax.validation.Valid;





import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class Cat extends Animal  {
  
  private @Valid String className = null;
  private @Valid String color = "red";
  private @Valid Boolean declawed = null;

  
  /**
   
   
   
   **/
  public Cat className(String className) {
    this.className = className;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("className")

  @NotNull

  public String getClassName() {
    return className;
  }
  public void setClassName(String className) {
    this.className = className;
  }

  
  /**
   
   
   
   **/
  public Cat color(String color) {
    this.color = color;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("color")

  public String getColor() {
    return color;
  }
  public void setColor(String color) {
    this.color = color;
  }

  
  /**
   
   
   
   **/
  public Cat declawed(Boolean declawed) {
    this.declawed = declawed;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("declawed")

  public Boolean isisDeclawed() {
    return declawed;
  }
  public void setDeclawed(Boolean declawed) {
    this.declawed = declawed;
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
    return Objects.equals(className, cat.className) &&
        Objects.equals(color, cat.color) &&
        Objects.equals(declawed, cat.declawed);
  }

  @Override
  public int hashCode() {
    return Objects.hash(className, color, declawed);
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}



