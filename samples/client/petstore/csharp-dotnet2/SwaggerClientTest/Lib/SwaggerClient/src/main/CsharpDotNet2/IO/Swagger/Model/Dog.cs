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
  public class Dog : Pet {
    /// <summary>
    /// Gets or Sets Bark
    /// </summary>
    [DataMember(Name="bark", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "bark")]
    public bool? Bark { get; set; }

    /// <summary>
    /// Gets or Sets Breed
    /// </summary>
    [DataMember(Name="breed", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "breed")]
    public string Breed { get; set; }


    /// <summary>
    /// Get the string presentation of the object
    /// </summary>
    /// <returns>String presentation of the object</returns>
    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Dog {\n");
      sb.Append("  Bark: ").Append(Bark).Append("\n");
      sb.Append("  Breed: ").Append(Breed).Append("\n");
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
