package io.swagger.model;

import io.swagger.model.Category;
import io.swagger.model.User;

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

public class AllOfSubCategoryCategory extends Category  {
  
  @Schema(description = "")
  private Boolean foo = null;
  
  @Schema(description = "")
  private Integer bar = null;
  
  @Schema(description = "")
  private String beer = null;
  
  @Schema(description = "")
  private User drunk = null;
 /**
   * Get foo
   * @return foo
  **/
  @JsonProperty("foo")
  public Boolean isFoo() {
    return foo;
  }

  public void setFoo(Boolean foo) {
    this.foo = foo;
  }

  public AllOfSubCategoryCategory foo(Boolean foo) {
    this.foo = foo;
    return this;
  }

 /**
   * Get bar
   * @return bar
  **/
  @JsonProperty("bar")
  public Integer getBar() {
    return bar;
  }

  public void setBar(Integer bar) {
    this.bar = bar;
  }

  public AllOfSubCategoryCategory bar(Integer bar) {
    this.bar = bar;
    return this;
  }

 /**
   * Get beer
   * @return beer
  **/
  @JsonProperty("beer")
  public String getBeer() {
    return beer;
  }

  public void setBeer(String beer) {
    this.beer = beer;
  }

  public AllOfSubCategoryCategory beer(String beer) {
    this.beer = beer;
    return this;
  }

 /**
   * Get drunk
   * @return drunk
  **/
  @JsonProperty("drunk")
  public User getDrunk() {
    return drunk;
  }

  public void setDrunk(User drunk) {
    this.drunk = drunk;
  }

  public AllOfSubCategoryCategory drunk(User drunk) {
    this.drunk = drunk;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class AllOfSubCategoryCategory {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    foo: ").append(toIndentedString(foo)).append("\n");
    sb.append("    bar: ").append(toIndentedString(bar)).append("\n");
    sb.append("    beer: ").append(toIndentedString(beer)).append("\n");
    sb.append("    drunk: ").append(toIndentedString(drunk)).append("\n");
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
