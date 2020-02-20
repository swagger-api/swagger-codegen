package io.swagger.model;

import java.util.Objects;
import java.util.ArrayList;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


public class PartMaster   {
  private OneOfPartMasterDestination destination = null;
  private OneOfPartMasterOrigin origin = null;

  /**
   **/
  
  @Schema(description = "")
  @JsonProperty("destination")
  public OneOfPartMasterDestination getDestination() {
    return destination;
  }
  public void setDestination(OneOfPartMasterDestination destination) {
    this.destination = destination;
  }

  /**
   **/
  
  @Schema(description = "")
  @JsonProperty("origin")
  public OneOfPartMasterOrigin getOrigin() {
    return origin;
  }
  public void setOrigin(OneOfPartMasterOrigin origin) {
    this.origin = origin;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    PartMaster partMaster = (PartMaster) o;
    return Objects.equals(destination, partMaster.destination) &&
        Objects.equals(origin, partMaster.origin);
  }

  @Override
  public int hashCode() {
    return Objects.hash(destination, origin);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class PartMaster {\n");
    
    sb.append("    destination: ").append(toIndentedString(destination)).append("\n");
    sb.append("    origin: ").append(toIndentedString(origin)).append("\n");
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
