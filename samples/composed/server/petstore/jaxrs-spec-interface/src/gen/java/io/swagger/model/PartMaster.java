package io.swagger.model;

import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class PartMaster   {
  private @Valid OneOfPartMasterDestination destination = null;
  private @Valid OneOfPartMasterOrigin origin = null;

  /**
   **/
  public PartMaster destination(OneOfPartMasterDestination destination) {
    this.destination = destination;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("destination")

  public OneOfPartMasterDestination getDestination() {
    return destination;
  }
  public void setDestination(OneOfPartMasterDestination destination) {
    this.destination = destination;
  }

  /**
   **/
  public PartMaster origin(OneOfPartMasterOrigin origin) {
    this.origin = origin;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("origin")

  public OneOfPartMasterOrigin getOrigin() {
    return origin;
  }
  public void setOrigin(OneOfPartMasterOrigin origin) {
    this.origin = origin;
  }


  @Override
  public boolean equals(java.lang.Object o) {
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}
