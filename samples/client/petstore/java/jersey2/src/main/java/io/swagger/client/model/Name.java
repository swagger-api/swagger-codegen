package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;



<<<<<<< HEAD


@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-03-17T16:00:50.657+08:00")
public class Name   {
  
  private Integer name = null;
=======
/**
 * Model for testing model name same as property name
 **/

@ApiModel(description = "Model for testing model name same as property name")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-04-13T14:36:32.196+02:00")
public class Name   {
  
  private Integer name = null;
  private Integer snakeCase = null;
>>>>>>> upstream/master

  
  /**
   **/
  public Name name(Integer name) {
    this.name = name;
    return this;
  }
  
<<<<<<< HEAD
  @ApiModelProperty(example = "null", value = "")
=======
  @ApiModelProperty(example = "null", required = true, value = "")
>>>>>>> upstream/master
  @JsonProperty("name")
  public Integer getName() {
    return name;
  }
  public void setName(Integer name) {
    this.name = name;
  }

<<<<<<< HEAD
  
=======

  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("snake_case")
  public Integer getSnakeCase() {
    return snakeCase;
  }

>>>>>>> upstream/master

  @Override
  public boolean equals(java.lang.Object o) {
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

