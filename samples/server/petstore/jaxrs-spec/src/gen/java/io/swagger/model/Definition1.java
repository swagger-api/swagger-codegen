package io.swagger.model;

import io.swagger.annotations.ApiModel;
import io.swagger.model.Definition2;
import javax.validation.constraints.*;


/**
 * Definition 2
 **/
import io.swagger.annotations.*;
import java.util.Objects;
@ApiModel(description = "Definition 2")

public class Definition1   {
  
  private Definition2 definition2 = null;
  private String id = null;

  /**
   **/
  public Definition1 definition2(Definition2 definition2) {
    this.definition2 = definition2;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public Definition2 getDefinition2() {
    return definition2;
  }
  public void setDefinition2(Definition2 definition2) {
    this.definition2 = definition2;
  }

  /**
   * Content Id for lookup
   **/
  public Definition1 id(String id) {
    this.id = id;
    return this;
  }

  
  @ApiModelProperty(value = "Content Id for lookup")
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Definition1 definition1 = (Definition1) o;
    return Objects.equals(definition2, definition1.definition2) &&
        Objects.equals(id, definition1.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(definition2, id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Definition1 {\n");
    
    sb.append("    definition2: ").append(toIndentedString(definition2)).append("\n");
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
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
