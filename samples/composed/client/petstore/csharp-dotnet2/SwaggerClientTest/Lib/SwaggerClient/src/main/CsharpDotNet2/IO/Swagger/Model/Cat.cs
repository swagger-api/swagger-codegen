using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization;
using Newtonsoft.Json;

namespace IO.Swagger.Model {

  /// <summary>
  /// 
  /// </summary>
  [DataContract]
  public class Cat : Pet {
    /// <summary>
    /// Gets or Sets Hunts
    /// </summary>
    [DataMember(Name="hunts", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "hunts")]
    public bool? Hunts { get; set; }

    /// <summary>
    /// Gets or Sets Age
    /// </summary>
    [DataMember(Name="age", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "age")]
    public int? Age { get; set; }


    /// <summary>
    /// Get the string presentation of the object
    /// </summary>
    /// <returns>String presentation of the object</returns>
    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Cat {\n");
      sb.Append("  Hunts: ").Append(Hunts).Append("\n");
      sb.Append("  Age: ").Append(Age).Append("\n");
      sb.Append("}\n");
      return sb.ToString();
    }

    /// <summary>
    /// Get the JSON string presentation of the object
    /// </summary>
    /// <returns>JSON string presentation of the object</returns>
    public  new string ToJson() {
      return JsonConvert.SerializeObject(this, Formatting.Indented);
    }

}
}
