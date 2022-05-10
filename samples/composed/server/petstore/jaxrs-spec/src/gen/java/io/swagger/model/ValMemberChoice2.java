package io.swagger.model;

import io.swagger.v3.oas.annotations.media.Schema;
import org.joda.time.LocalDate;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
 * Business Term: Scheme Member Definition: Information about a Member of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.
 **/
import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
@Schema(description = "Business Term: Scheme Member Definition: Information about a Member of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.")

public class ValMemberChoice2  implements Serializable , OneOfvalMembersValMemberItems  {
  private @Valid LocalDate valLeavingDate = null;

  /**
   * Business Term: Leaving Date Definition: The date the member left/is due to leave the scheme. Purpose: To identify those members that have left or are due to leave the scheme. To be able to calculate the benefit insured/premium payable in respect of the member/category.
   **/
  public ValMemberChoice2 valLeavingDate(LocalDate valLeavingDate) {
    this.valLeavingDate = valLeavingDate;
    return this;
  }

  
  @ApiModelProperty(value = "Business Term: Leaving Date Definition: The date the member left/is due to leave the scheme. Purpose: To identify those members that have left or are due to leave the scheme. To be able to calculate the benefit insured/premium payable in respect of the member/category.")
  @JsonProperty("val_leaving_date")

  public LocalDate getValLeavingDate() {
    return valLeavingDate;
  }
  public void setValLeavingDate(LocalDate valLeavingDate) {
    this.valLeavingDate = valLeavingDate;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ValMemberChoice2 valMemberChoice2 = (ValMemberChoice2) o;
    return Objects.equals(valLeavingDate, valMemberChoice2.valLeavingDate);
  }

  @Override
  public int hashCode() {
    return Objects.hash(valLeavingDate);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ValMemberChoice2 {\n");
    
    sb.append("    valLeavingDate: ").append(toIndentedString(valLeavingDate)).append("\n");
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
