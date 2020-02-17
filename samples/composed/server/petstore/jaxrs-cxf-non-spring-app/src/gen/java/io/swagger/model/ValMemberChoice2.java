package io.swagger.model;

import io.swagger.v3.oas.annotations.media.Schema;
import org.joda.time.LocalDate;
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
  * Business Term: Scheme Member Definition: Information about a Member of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.
 **/
@Schema(description="Business Term: Scheme Member Definition: Information about a Member of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.")
public class ValMemberChoice2   {
  
  @Schema(description = "Business Term: Leaving Date Definition: The date the member left/is due to leave the scheme. Purpose: To identify those members that have left or are due to leave the scheme. To be able to calculate the benefit insured/premium payable in respect of the member/category.")
 /**
   * Business Term: Leaving Date Definition: The date the member left/is due to leave the scheme. Purpose: To identify those members that have left or are due to leave the scheme. To be able to calculate the benefit insured/premium payable in respect of the member/category.  
  **/
  private LocalDate valLeavingDate = null;
 /**
   * Business Term: Leaving Date Definition: The date the member left/is due to leave the scheme. Purpose: To identify those members that have left or are due to leave the scheme. To be able to calculate the benefit insured/premium payable in respect of the member/category.
   * @return valLeavingDate
  **/
  @JsonProperty("val_leaving_date")
  public LocalDate getValLeavingDate() {
    return valLeavingDate;
  }

  public void setValLeavingDate(LocalDate valLeavingDate) {
    this.valLeavingDate = valLeavingDate;
  }

  public ValMemberChoice2 valLeavingDate(LocalDate valLeavingDate) {
    this.valLeavingDate = valLeavingDate;
    return this;
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
  private static String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}
