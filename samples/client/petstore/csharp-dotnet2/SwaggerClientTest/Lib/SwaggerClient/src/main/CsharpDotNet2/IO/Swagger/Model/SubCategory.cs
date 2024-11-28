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
  public class SubCategory {
    /// <summary>
    /// Gets or Sets Category
    /// </summary>
    [DataMember(Name="category", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "category")]
    public AllOfSubCategoryCategory Category { get; set; }

    /// <summary>
    /// Gets or Sets Category2
    /// </summary>
    [DataMember(Name="category2", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "category2")]
    public Category Category2 { get; set; }

    /// <summary>
    /// Gets or Sets Pets
    /// </summary>
    [DataMember(Name="pets", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "pets")]
    public List<AllOfSubCategoryPetsItems> Pets { get; set; }


    /// <summary>
    /// Get the string presentation of the object
    /// </summary>
    /// <returns>String presentation of the object</returns>
    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class SubCategory {\n");
      sb.Append("  Category: ").Append(Category).Append("\n");
      sb.Append("  Category2: ").Append(Category2).Append("\n");
      sb.Append("  Pets: ").Append(Pets).Append("\n");
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
