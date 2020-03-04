package io.swagger.model;

import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class NullableEnumModel  implements Serializable  {
public enum EnumPropEnum {

    A(String.valueOf("a")), B(String.valueOf("b")), NULL(null);


    private String value;

    EnumPropEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static EnumPropEnum fromValue(String v) {
        for (EnumPropEnum b : EnumPropEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}
  private @Valid EnumPropEnum enumProp = null;

  /**
   **/
  public NullableEnumModel enumProp(EnumPropEnum enumProp) {
    this.enumProp = enumProp;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("enumProp")

  public EnumPropEnum getEnumProp() {
    return enumProp;
  }
  public void setEnumProp(EnumPropEnum enumProp) {
    this.enumProp = enumProp;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    NullableEnumModel nullableEnumModel = (NullableEnumModel) o;
    return Objects.equals(enumProp, nullableEnumModel.enumProp);
  }

  @Override
  public int hashCode() {
    return Objects.hash(enumProp);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class NullableEnumModel {\n");
    
    sb.append("    enumProp: ").append(toIndentedString(enumProp)).append("\n");
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
