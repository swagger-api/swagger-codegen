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
  public class AllOfSubCategoryCategory : Category {
    /// <summary>
    /// Gets or Sets Foo
    /// </summary>
    [DataMember(Name="foo", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "foo")]
    public bool? Foo { get; set; }

    /// <summary>
    /// Gets or Sets Bar
    /// </summary>
    [DataMember(Name="bar", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "bar")]
    public int? Bar { get; set; }

    /// <summary>
    /// Gets or Sets Beer
    /// </summary>
    [DataMember(Name="beer", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "beer")]
    public string Beer { get; set; }

    /// <summary>
    /// Gets or Sets Drunk
    /// </summary>
    [DataMember(Name="drunk", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "drunk")]
    public User Drunk { get; set; }


    /// <summary>
    /// Get the string presentation of the object
    /// </summary>
    /// <returns>String presentation of the object</returns>
    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class AllOfSubCategoryCategory {\n");
      sb.Append("  Foo: ").Append(Foo).Append("\n");
      sb.Append("  Bar: ").Append(Bar).Append("\n");
      sb.Append("  Beer: ").Append(Beer).Append("\n");
      sb.Append("  Drunk: ").Append(Drunk).Append("\n");
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
