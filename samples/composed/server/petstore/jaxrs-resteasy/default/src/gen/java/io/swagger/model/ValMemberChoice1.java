package io.swagger.model;

import java.util.Objects;
import java.util.ArrayList;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.Date;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;

@Schema(description="Business Term: Scheme Member Definition: Information about a Member of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.")
public class ValMemberChoice1  implements OneOfvalMembersValMemberItems  {
  private Date valDateOfBirth = null;  private Date valLeavingDate = null;

  /**
   * Business Term: Date Of Birth Definition: The date of birth of the member. Purpose: To be able to uniquely identify a member within a scheme.
   **/
  
  @Schema(required = true, description = "Business Term: Date Of Birth Definition: The date of birth of the member. Purpose: To be able to uniquely identify a member within a scheme.")
  @JsonProperty("val_date_of_birth")
  @NotNull
  public Date getValDateOfBirth() {
    return valDateOfBirth;
  }
  public void setValDateOfBirth(Date valDateOfBirth) {
    this.valDateOfBirth = valDateOfBirth;
  }

  /**
   * Business Term: Leaving Date Definition: The date the member left/is due to leave the scheme. Purpose: To identify those members that have left or are due to leave the scheme. To be able to calculate the benefit insured/premium payable in respect of the member/category.
   **/
  
  @Schema(description = "Business Term: Leaving Date Definition: The date the member left/is due to leave the scheme. Purpose: To identify those members that have left or are due to leave the scheme. To be able to calculate the benefit insured/premium payable in respect of the member/category.")
  @JsonProperty("val_leaving_date")
  public Date getValLeavingDate() {
    return valLeavingDate;
  }
  public void setValLeavingDate(Date valLeavingDate) {
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
    ValMemberChoice1 valMemberChoice1 = (ValMemberChoice1) o;
    return Objects.equals(valDateOfBirth, valMemberChoice1.valDateOfBirth) &&
        Objects.equals(valLeavingDate, valMemberChoice1.valLeavingDate);
  }

  @Override
  public int hashCode() {
    return Objects.hash(valDateOfBirth, valLeavingDate);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ValMemberChoice1 {\n");
    
    sb.append("    valDateOfBirth: ").append(toIndentedString(valDateOfBirth)).append("\n");
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
