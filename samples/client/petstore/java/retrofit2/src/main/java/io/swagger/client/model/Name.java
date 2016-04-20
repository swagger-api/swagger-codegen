package io.swagger.client.model;

import java.util.Objects;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

<<<<<<< HEAD

=======
>>>>>>> upstream/master
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
  
  @SerializedName("snake_case")
  private Integer snakeCase = null;
  

  
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
>>>>>>> upstream/master
  /**
   **/
  @ApiModelProperty(value = "")
  public Integer getSnakeCase() {
    return snakeCase;
  }
<<<<<<< HEAD
  public void setSnakeCase(Integer snakeCase) {
    this.snakeCase = snakeCase;
  }

  
=======

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
    return Objects.equals(name, name.name) &&
        Objects.equals(snakeCase, name.snakeCase);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, snakeCase);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Name {\n");
    
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    snakeCase: ").append(toIndentedString(snakeCase)).append("\n");
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
<<<<<<< HEAD


=======
>>>>>>> upstream/master
