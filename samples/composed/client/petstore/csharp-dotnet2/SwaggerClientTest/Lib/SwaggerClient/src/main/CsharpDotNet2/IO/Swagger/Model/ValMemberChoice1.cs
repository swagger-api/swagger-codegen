using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization;
using Newtonsoft.Json;

namespace IO.Swagger.Model {

  /// <summary>
  /// Business Term: Scheme Member Definition: Information about a Member of the Scheme. Purpose: To have enough information to be able to produce a Statement of Account indicating premium due.
  /// </summary>
  [DataContract]
  public class ValMemberChoice1 {
    /// <summary>
    /// Business Term: Date Of Birth Definition: The date of birth of the member. Purpose: To be able to uniquely identify a member within a scheme.
    /// </summary>
    /// <value>Business Term: Date Of Birth Definition: The date of birth of the member. Purpose: To be able to uniquely identify a member within a scheme.</value>
    [DataMember(Name="val_date_of_birth", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "val_date_of_birth")]
    public DateTime? ValDateOfBirth { get; set; }

    /// <summary>
    /// Business Term: Leaving Date Definition: The date the member left/is due to leave the scheme. Purpose: To identify those members that have left or are due to leave the scheme. To be able to calculate the benefit insured/premium payable in respect of the member/category.
    /// </summary>
    /// <value>Business Term: Leaving Date Definition: The date the member left/is due to leave the scheme. Purpose: To identify those members that have left or are due to leave the scheme. To be able to calculate the benefit insured/premium payable in respect of the member/category.</value>
    [DataMember(Name="val_leaving_date", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "val_leaving_date")]
    public DateTime? ValLeavingDate { get; set; }


    /// <summary>
    /// Get the string presentation of the object
    /// </summary>
    /// <returns>String presentation of the object</returns>
    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class ValMemberChoice1 {\n");
      sb.Append("  ValDateOfBirth: ").Append(ValDateOfBirth).Append("\n");
      sb.Append("  ValLeavingDate: ").Append(ValLeavingDate).Append("\n");
      sb.Append("}\n");
      return sb.ToString();
    }

    /// <summary>
    /// Get the JSON string presentation of the object
    /// </summary>
    /// <returns>JSON string presentation of the object</returns>
    public string ToJson() {
      return JsonConvert.SerializeObject(this, Formatting.Indented);
    }

}
}
