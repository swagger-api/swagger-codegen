package io.swagger.model;

import java.util.Objects;
import java.util.ArrayList;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


public class PartFour  implements OneOfPartMasterOrigin  {
  private String otherIdPart = null;

  /**
   **/
  
  @Schema(description = "")
  @JsonProperty("otherIdPart")
  public String getOtherIdPart() {
    return otherIdPart;
  }
  public void setOtherIdPart(String otherIdPart) {
    this.otherIdPart = otherIdPart;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    PartFour partFour = (PartFour) o;
    return Objects.equals(otherIdPart, partFour.otherIdPart);
  }

  @Override
  public int hashCode() {
    return Objects.hash(otherIdPart);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class PartFour {\n");
    
    sb.append("    otherIdPart: ").append(toIndentedString(otherIdPart)).append("\n");
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
