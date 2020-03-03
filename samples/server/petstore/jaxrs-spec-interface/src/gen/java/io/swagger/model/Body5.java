package io.swagger.model;

import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class Body5   {
  private @Valid String param = null;
  private @Valid String param2 = null;

  /**
   * field1
   **/
  public Body5 param(String param) {
    this.param = param;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "field1")
  @JsonProperty("param")
  @NotNull

  public String getParam() {
    return param;
  }
  public void setParam(String param) {
    this.param = param;
  }

  /**
   * field2
   **/
  public Body5 param2(String param2) {
    this.param2 = param2;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "field2")
  @JsonProperty("param2")
  @NotNull

  public String getParam2() {
    return param2;
  }
  public void setParam2(String param2) {
    this.param2 = param2;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Body5 body5 = (Body5) o;
    return Objects.equals(param, body5.param) &&
        Objects.equals(param2, body5.param2);
  }

  @Override
  public int hashCode() {
    return Objects.hash(param, param2);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Body5 {\n");
    
    sb.append("    param: ").append(toIndentedString(param)).append("\n");
    sb.append("    param2: ").append(toIndentedString(param2)).append("\n");
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
