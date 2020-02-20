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
 * Body1
 */
@Validated
public class Body1   {
  @JsonProperty("parrots")
  @Valid
  private List<AnyOfbody1ParrotsItems> parrots = null;

  public Body1 parrots(List<AnyOfbody1ParrotsItems> parrots) {
    this.parrots = parrots;
    return this;
  }

  public Body1 addParrotsItem(AnyOfbody1ParrotsItems parrotsItem) {
    if (this.parrots == null) {
      this.parrots = new ArrayList<AnyOfbody1ParrotsItems>();
    }
    this.parrots.add(parrotsItem);
    return this;
  }

  /**
   * Get parrots
   * @return parrots
  **/
  @ApiModelProperty(value = "")
  
    public List<AnyOfbody1ParrotsItems> getParrots() {
    return parrots;
  }

  public void setParrots(List<AnyOfbody1ParrotsItems> parrots) {
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
    Body1 body1 = (Body1) o;
    return Objects.equals(this.parrots, body1.parrots);
  }

  @Override
  public int hashCode() {
    return Objects.hash(parrots);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Body1 {\n");
    
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
