package io.swagger.model;


import io.swagger.annotations.ApiModelProperty;

public class SpecialModelName  {
  
  @ApiModelProperty(example = "null", value = "")
  private Long specialPropertyName = null;

 /**
   * Get specialPropertyName
   * @return specialPropertyName
  **/
  public Long getSpecialPropertyName() {
    return specialPropertyName;
  }

  public void setSpecialPropertyName(Long specialPropertyName) {
    this.specialPropertyName = specialPropertyName;
  }

  public SpecialModelName specialPropertyName(Long specialPropertyName) {
    this.specialPropertyName = specialPropertyName;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SpecialModelName {\n");
    
    sb.append("    specialPropertyName: ").append(toIndentedString(specialPropertyName)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

