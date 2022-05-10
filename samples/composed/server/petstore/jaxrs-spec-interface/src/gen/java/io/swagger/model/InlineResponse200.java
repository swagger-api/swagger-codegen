package io.swagger.model;

import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class InlineResponse200   {
  private @Valid List<OneOfinlineResponse200ParrotsItems> parrots = new ArrayList<OneOfinlineResponse200ParrotsItems>();

  /**
   **/
  public InlineResponse200 parrots(List<OneOfinlineResponse200ParrotsItems> parrots) {
    this.parrots = parrots;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("parrots")

  public List<OneOfinlineResponse200ParrotsItems> getParrots() {
    return parrots;
  }
  public void setParrots(List<OneOfinlineResponse200ParrotsItems> parrots) {
    this.parrots = parrots;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    InlineResponse200 inlineResponse200 = (InlineResponse200) o;
    return Objects.equals(parrots, inlineResponse200.parrots);
  }

  @Override
  public int hashCode() {
    return Objects.hash(parrots);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class InlineResponse200 {\n");
    
    sb.append("    parrots: ").append(toIndentedString(parrots)).append("\n");
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
