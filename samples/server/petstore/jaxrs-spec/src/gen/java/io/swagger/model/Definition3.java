package io.swagger.model;

import io.swagger.annotations.ApiModel;
import io.swagger.model.Definition3;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;


/**
 * Definition 3 (recursive)
 **/
import io.swagger.annotations.*;
import java.util.Objects;
@ApiModel(description = "Definition 3 (recursive)")

public class Definition3   {
  
  private List<Definition3> definition3 = new ArrayList<Definition3>();
  private String id = null;

  /**
   * nested Definition3 array
   **/
  public Definition3 definition3(List<Definition3> definition3) {
    this.definition3 = definition3;
    return this;
  }

  
  @ApiModelProperty(value = "nested Definition3 array")
  public List<Definition3> getDefinition3() {
    return definition3;
  }
  public void setDefinition3(List<Definition3> definition3) {
    this.definition3 = definition3;
  }

  /**
   * Content Id for lookup
   **/
  public Definition3 id(String id) {
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
    Definition3 definition3 = (Definition3) o;
    return Objects.equals(definition3, definition3.definition3) &&
        Objects.equals(id, definition3.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(definition3, id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Definition3 {\n");
    
    sb.append("    definition3: ").append(toIndentedString(definition3)).append("\n");
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
