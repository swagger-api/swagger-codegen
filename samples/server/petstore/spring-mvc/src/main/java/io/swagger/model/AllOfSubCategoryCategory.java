package io.swagger.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.model.Category;
import io.swagger.model.User;
import org.springframework.validation.annotation.Validated;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * AllOfSubCategoryCategory
 */
@Validated



public class AllOfSubCategoryCategory extends Category  {
  @JsonProperty("foo")
  private Boolean foo = null;

  @JsonProperty("bar")
  private Integer bar = null;

  @JsonProperty("beer")
  private String beer = null;

  @JsonProperty("drunk")
  private User drunk = null;

  public AllOfSubCategoryCategory foo(Boolean foo) {
    this.foo = foo;
    return this;
  }

  /**
   * Get foo
   * @return foo
   **/
  @ApiModelProperty(value = "")
  
    public Boolean isFoo() {
    return foo;
  }

  public void setFoo(Boolean foo) {
    this.foo = foo;
  }

  public AllOfSubCategoryCategory bar(Integer bar) {
    this.bar = bar;
    return this;
  }

  /**
   * Get bar
   * @return bar
   **/
  @ApiModelProperty(value = "")
  
    public Integer getBar() {
    return bar;
  }

  public void setBar(Integer bar) {
    this.bar = bar;
  }

  public AllOfSubCategoryCategory beer(String beer) {
    this.beer = beer;
    return this;
  }

  /**
   * Get beer
   * @return beer
   **/
  @ApiModelProperty(value = "")
  
    public String getBeer() {
    return beer;
  }

  public void setBeer(String beer) {
    this.beer = beer;
  }

  public AllOfSubCategoryCategory drunk(User drunk) {
    this.drunk = drunk;
    return this;
  }

  /**
   * Get drunk
   * @return drunk
   **/
  @ApiModelProperty(value = "")
  
    @Valid
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
    return Objects.equals(this.foo, allOfSubCategoryCategory.foo) &&
        Objects.equals(this.bar, allOfSubCategoryCategory.bar) &&
        Objects.equals(this.beer, allOfSubCategoryCategory.beer) &&
        Objects.equals(this.drunk, allOfSubCategoryCategory.drunk) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(foo, bar, beer, drunk, super.hashCode());
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
