package io.swagger.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.List;
import org.springframework.validation.annotation.Validated;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * InlineResponse200
 */
@Validated
public class InlineResponse200   {
  @JsonProperty("parrots")
  @Valid
  private List<OneOfinlineResponse200ParrotsItems> parrots = null;

  public InlineResponse200 parrots(List<OneOfinlineResponse200ParrotsItems> parrots) {
    this.parrots = parrots;
    return this;
  }

  public InlineResponse200 addParrotsItem(OneOfinlineResponse200ParrotsItems parrotsItem) {
    if (this.parrots == null) {
      this.parrots = new ArrayList<OneOfinlineResponse200ParrotsItems>();
    }
    this.parrots.add(parrotsItem);
    return this;
  }

  /**
   * Get parrots
   * @return parrots
  **/
  @ApiModelProperty(value = "")
  
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
    return Objects.equals(this.parrots, inlineResponse200.parrots);
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
