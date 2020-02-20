package io.swagger.model;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;




public class Body1   {
  @JsonProperty("parrots")
  private List<AnyOfbody1ParrotsItems> parrots = null;
  /**
   **/
  public Body1 parrots(List<AnyOfbody1ParrotsItems> parrots) {
    this.parrots = parrots;
    return this;
  }

  
  @Schema(description = "")
  @JsonProperty("parrots")
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
    return Objects.equals(parrots, body1.parrots);
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
