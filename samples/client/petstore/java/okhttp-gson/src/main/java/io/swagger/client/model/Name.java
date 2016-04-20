package io.swagger.client.model;

import java.util.Objects;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import com.google.gson.annotations.SerializedName;




<<<<<<< HEAD

=======
/**
 * Model for testing model name same as property name
 **/
@ApiModel(description = "Model for testing model name same as property name")
>>>>>>> upstream/master
public class Name   {
  
  @SerializedName("name")
  private Integer name = null;
<<<<<<< HEAD
  

  
  /**
   **/
  @ApiModelProperty(value = "")
=======

  @SerializedName("snake_case")
  private Integer snakeCase = null;

  /**
   **/
  @ApiModelProperty(required = true, value = "")
>>>>>>> upstream/master
  public Integer getName() {
    return name;
  }
  public void setName(Integer name) {
    this.name = name;
  }

<<<<<<< HEAD
  
=======
  /**
   **/
  @ApiModelProperty(value = "")
  public Integer getSnakeCase() {
    return snakeCase;
  }

>>>>>>> upstream/master

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Name name = (Name) o;
<<<<<<< HEAD
    return Objects.equals(this.name, name.name);
=======
    return Objects.equals(this.name, name.name) &&
        Objects.equals(this.snakeCase, name.snakeCase);
>>>>>>> upstream/master
  }

  @Override
  public int hashCode() {
<<<<<<< HEAD
    return Objects.hash(name);
=======
    return Objects.hash(name, snakeCase);
>>>>>>> upstream/master
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Name {\n");
    
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
<<<<<<< HEAD
=======
    sb.append("    snakeCase: ").append(toIndentedString(snakeCase)).append("\n");
>>>>>>> upstream/master
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
