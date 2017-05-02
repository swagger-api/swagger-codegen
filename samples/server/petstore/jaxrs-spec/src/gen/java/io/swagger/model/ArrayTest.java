package io.swagger.model;

import io.swagger.model.ReadOnlyFirst;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;


import io.swagger.annotations.*;
import java.util.Objects;


public class ArrayTest   {
  
  private List<String> arrayOfString = new ArrayList<String>();
  private List<List<Long>> arrayArrayOfInteger = new ArrayList<List<Long>>();
  private List<List<ReadOnlyFirst>> arrayArrayOfModel = new ArrayList<List<ReadOnlyFirst>>();

public enum ArrayOfEnumEnum {

    UPPER(String.valueOf("UPPER")), LOWER(String.valueOf("lower"));


    private String value;

    ArrayOfEnumEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static ArrayOfEnumEnum fromValue(String v) {
        for (ArrayOfEnumEnum b : ArrayOfEnumEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}

  private List<ArrayOfEnumEnum> arrayOfEnum = new ArrayList<ArrayOfEnumEnum>();

  /**
   **/
  public ArrayTest arrayOfString(List<String> arrayOfString) {
    this.arrayOfString = arrayOfString;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public List<String> getArrayOfString() {
    return arrayOfString;
  }
  public void setArrayOfString(List<String> arrayOfString) {
    this.arrayOfString = arrayOfString;
  }

  /**
   **/
  public ArrayTest arrayArrayOfInteger(List<List<Long>> arrayArrayOfInteger) {
    this.arrayArrayOfInteger = arrayArrayOfInteger;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public List<List<Long>> getArrayArrayOfInteger() {
    return arrayArrayOfInteger;
  }
  public void setArrayArrayOfInteger(List<List<Long>> arrayArrayOfInteger) {
    this.arrayArrayOfInteger = arrayArrayOfInteger;
  }

  /**
   **/
  public ArrayTest arrayArrayOfModel(List<List<ReadOnlyFirst>> arrayArrayOfModel) {
    this.arrayArrayOfModel = arrayArrayOfModel;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public List<List<ReadOnlyFirst>> getArrayArrayOfModel() {
    return arrayArrayOfModel;
  }
  public void setArrayArrayOfModel(List<List<ReadOnlyFirst>> arrayArrayOfModel) {
    this.arrayArrayOfModel = arrayArrayOfModel;
  }

  /**
   **/
  public ArrayTest arrayOfEnum(List<ArrayOfEnumEnum> arrayOfEnum) {
    this.arrayOfEnum = arrayOfEnum;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public List<ArrayOfEnumEnum> getArrayOfEnum() {
    return arrayOfEnum;
  }
  public void setArrayOfEnum(List<ArrayOfEnumEnum> arrayOfEnum) {
    this.arrayOfEnum = arrayOfEnum;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ArrayTest arrayTest = (ArrayTest) o;
    return Objects.equals(arrayOfString, arrayTest.arrayOfString) &&
        Objects.equals(arrayArrayOfInteger, arrayTest.arrayArrayOfInteger) &&
        Objects.equals(arrayArrayOfModel, arrayTest.arrayArrayOfModel) &&
        Objects.equals(arrayOfEnum, arrayTest.arrayOfEnum);
  }

  @Override
  public int hashCode() {
    return Objects.hash(arrayOfString, arrayArrayOfInteger, arrayArrayOfModel, arrayOfEnum);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ArrayTest {\n");
    
    sb.append("    arrayOfString: ").append(toIndentedString(arrayOfString)).append("\n");
    sb.append("    arrayArrayOfInteger: ").append(toIndentedString(arrayArrayOfInteger)).append("\n");
    sb.append("    arrayArrayOfModel: ").append(toIndentedString(arrayArrayOfModel)).append("\n");
    sb.append("    arrayOfEnum: ").append(toIndentedString(arrayOfEnum)).append("\n");
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
