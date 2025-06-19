package io.swagger.model;

import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class ModelList  implements Serializable {
  
  private @Valid String _123List = null;

  /**
   **/
  public ModelList _123List(String _123List) {
    this._123List = _123List;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("123-list")
  public String get123List() {
    return _123List;
  }
  public void set123List(String _123List) {
    this._123List = _123List;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ModelList _list = (ModelList) o;
    return Objects.equals(_123List, _list._123List);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_123List);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ModelList {\n");
    
    sb.append("    _123List: ").append(toIndentedString(_123List)).append("\n");
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

