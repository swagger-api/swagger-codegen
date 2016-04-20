package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;



<<<<<<< HEAD




@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-03-19T15:53:31.820+08:00")
=======
/**
 * Model for testing model name same as property name
 **/

@ApiModel(description = "Model for testing model name same as property name")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-04-13T14:36:33.302+02:00")
>>>>>>> upstream/master
public class Name   {
  
  private Integer name = null;
  private Integer snakeCase = null;

  
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
  
  /**
   **/
  public Name snakeCase(Integer snakeCase) {
    this.snakeCase = snakeCase;
    return this;
  }
  
=======

>>>>>>> upstream/master
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("snake_case")
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
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Name name = (Name) o;
    return Objects.equals(this.name, name.name) &&
        Objects.equals(this.snakeCase, name.snakeCase);
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

<<<<<<< HEAD


=======
>>>>>>> upstream/master
