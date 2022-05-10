package io.swagger.model;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;

import io.swagger.v3.oas.annotations.media.Schema;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonCreator;

/**
  * Business Term: Scheme Members Definition: Information about the Members of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.
 **/
@Schema(description="Business Term: Scheme Members Definition: Information about the Members of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.")
public class ValMembers   {
  
  @Schema(required = true, description = "Business Term: Scheme Member Definition: Information about a Member of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.")
 /**
   * Business Term: Scheme Member Definition: Information about a Member of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.  
  **/
  private List<OneOfvalMembersValMemberItems> valMember = new ArrayList<OneOfvalMembersValMemberItems>();
 /**
   * Business Term: Scheme Member Definition: Information about a Member of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.
   * @return valMember
  **/
  @JsonProperty("val_member")
  @NotNull
 @Size(min=1)  public List<OneOfvalMembersValMemberItems> getValMember() {
    return valMember;
  }

  public void setValMember(List<OneOfvalMembersValMemberItems> valMember) {
    this.valMember = valMember;
  }

  public ValMembers valMember(List<OneOfvalMembersValMemberItems> valMember) {
    this.valMember = valMember;
    return this;
  }

  public ValMembers addValMemberItem(OneOfvalMembersValMemberItems valMemberItem) {
    this.valMember.add(valMemberItem);
    return this;
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
  private static String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}
