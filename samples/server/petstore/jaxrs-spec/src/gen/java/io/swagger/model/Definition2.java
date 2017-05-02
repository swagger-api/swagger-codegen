package io.swagger.model;

import io.swagger.annotations.ApiModel;
import io.swagger.model.Definition1;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;


/**
 * Definition 2
 **/
import io.swagger.annotations.*;
import java.util.Objects;
@ApiModel(description = "Definition 2")

public class Definition2   {
  
  private List<Definition1> definition1 = new ArrayList<Definition1>();
  private String id = null;

  /**
   * nested Definition1 array
   **/
  public Definition2 definition1(List<Definition1> definition1) {
    this.definition1 = definition1;
    return this;
  }

  
  @ApiModelProperty(value = "nested Definition1 array")
  public List<Definition1> getDefinition1() {
    return definition1;
  }
  public void setDefinition1(List<Definition1> definition1) {
    this.definition1 = definition1;
  }

  /**
   * Content Id for lookup
   **/
  public Definition2 id(String id) {
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
    Definition2 definition2 = (Definition2) o;
    return Objects.equals(definition1, definition2.definition1) &&
        Objects.equals(id, definition2.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(definition1, id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Definition2 {\n");
    
    sb.append("    definition1: ").append(toIndentedString(definition1)).append("\n");
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
