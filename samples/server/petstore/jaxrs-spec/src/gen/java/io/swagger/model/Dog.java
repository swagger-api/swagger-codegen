package io.swagger.model;

import io.swagger.model.Animal;


import java.io.Serializable;


import javax.validation.constraints.*;
import javax.validation.Valid;





import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class Dog extends Animal implements Serializable {
  
  private @Valid String className = null;
  private @Valid String color = "red";
  private @Valid String breed = null;

  
  /**
   
   
   
   **/
  public Dog className(String className) {
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
  public Dog color(String color) {
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
  public Dog breed(String breed) {
    this.breed = breed;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("breed")

  public String getBreed() {
    return breed;
  }
  public void setBreed(String breed) {
    this.breed = breed;
  }

  

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Dog dog = (Dog) o;
    return Objects.equals(className, dog.className) &&
        Objects.equals(color, dog.color) &&
        Objects.equals(breed, dog.breed);
  }

  @Override
  public int hashCode() {
    return Objects.hash(className, color, breed);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Dog {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    className: ").append(toIndentedString(className)).append("\n");
    sb.append("    color: ").append(toIndentedString(color)).append("\n");
    sb.append("    breed: ").append(toIndentedString(breed)).append("\n");
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



