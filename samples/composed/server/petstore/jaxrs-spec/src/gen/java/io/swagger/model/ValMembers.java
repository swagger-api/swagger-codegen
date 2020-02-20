package io.swagger.model;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
 * Business Term: Scheme Members Definition: Information about the Members of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.
 **/
import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
@Schema(description = "Business Term: Scheme Members Definition: Information about the Members of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.")

public class ValMembers  implements Serializable  {
  private @Valid List<OneOfvalMembersValMemberItems> valMember = new ArrayList<OneOfvalMembersValMemberItems>();

  /**
   * Business Term: Scheme Member Definition: Information about a Member of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.
   **/
  public ValMembers valMember(List<OneOfvalMembersValMemberItems> valMember) {
    this.valMember = valMember;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "Business Term: Scheme Member Definition: Information about a Member of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.")
  @JsonProperty("val_member")
  @NotNull
 @Size(min=1)
  public List<OneOfvalMembersValMemberItems> getValMember() {
    return valMember;
  }
  public void setValMember(List<OneOfvalMembersValMemberItems> valMember) {
    this.valMember = valMember;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ValMembers valMembers = (ValMembers) o;
    return Objects.equals(valMember, valMembers.valMember);
  }

  @Override
  public int hashCode() {
    return Objects.hash(valMember);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ValMembers {\n");
    
    sb.append("    valMember: ").append(toIndentedString(valMember)).append("\n");
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
