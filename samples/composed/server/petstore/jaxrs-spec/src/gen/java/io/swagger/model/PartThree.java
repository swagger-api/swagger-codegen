package io.swagger.model;

import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class PartThree  implements Serializable , OneOfPartMasterOrigin  {
  private @Valid String otherId = null;

  /**
   **/
  public PartThree otherId(String otherId) {
    this.otherId = otherId;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("otherId")

  public String getOtherId() {
    return otherId;
  }
  public void setOtherId(String otherId) {
    this.otherId = otherId;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    PartThree partThree = (PartThree) o;
    return Objects.equals(otherId, partThree.otherId);
  }

  @Override
  public int hashCode() {
    return Objects.hash(otherId);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class PartThree {\n");
    
    sb.append("    otherId: ").append(toIndentedString(otherId)).append("\n");
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
