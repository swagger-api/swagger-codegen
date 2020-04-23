package io.swagger.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.model.Category;
import io.swagger.model.User;
import javax.validation.constraints.*;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.Objects;

import javax.xml.bind.annotation.*;



public class AllOfSubCategoryCategory extends Category  {
  private Boolean foo = null;
  private Integer bar = null;
  private String beer = null;
  private User drunk = null;

  /**
   **/
  public AllOfSubCategoryCategory foo(Boolean foo) {
    this.foo = foo;
    return this;
  }

  
  
  @Schema(description = "")
  @JsonProperty("foo")
  public Boolean isFoo() {
    return foo;
  }
  public void setFoo(Boolean foo) {
    this.foo = foo;
  }

  /**
   **/
  public AllOfSubCategoryCategory bar(Integer bar) {
    this.bar = bar;
    return this;
  }

  
  
  @Schema(description = "")
  @JsonProperty("bar")
  public Integer getBar() {
    return bar;
  }
  public void setBar(Integer bar) {
    this.bar = bar;
  }

  /**
   **/
  public AllOfSubCategoryCategory beer(String beer) {
    this.beer = beer;
    return this;
  }

  
  
  @Schema(description = "")
  @JsonProperty("beer")
  public String getBeer() {
    return beer;
  }
  public void setBeer(String beer) {
    this.beer = beer;
  }

  /**
   **/
  public AllOfSubCategoryCategory drunk(User drunk) {
    this.drunk = drunk;
    return this;
  }

  
  
  @Schema(description = "")
  @JsonProperty("drunk")
  public User getDrunk() {
    return drunk;
  }
  public void setDrunk(User drunk) {
    this.drunk = drunk;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    AllOfSubCategoryCategory allOfSubCategoryCategory = (AllOfSubCategoryCategory) o;
    return Objects.equals(foo, allOfSubCategoryCategory.foo) &&
        Objects.equals(bar, allOfSubCategoryCategory.bar) &&
        Objects.equals(beer, allOfSubCategoryCategory.beer) &&
        Objects.equals(drunk, allOfSubCategoryCategory.drunk);
  }

  @Override
  public int hashCode() {
    return Objects.hash(foo, bar, beer, drunk);
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}
