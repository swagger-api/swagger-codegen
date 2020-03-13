package io.swagger.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.model.Category;
import java.util.ArrayList;
import java.util.List;
import org.springframework.validation.annotation.Validated;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * SubCategory
 */
@Validated
public class SubCategory   {
  @JsonProperty("category")
  private AllOfSubCategoryCategory category = null;

  @JsonProperty("category2")
  private Category category2 = null;

  @JsonProperty("pets")
  @Valid
  private List<AllOfSubCategoryPetsItems> pets = null;

  public SubCategory category(AllOfSubCategoryCategory category) {
    this.category = category;
    return this;
  }

  /**
   * Get category
   * @return category
  **/
  @ApiModelProperty(value = "")
  
    public AllOfSubCategoryCategory getCategory() {
    return category;
  }

  public void setCategory(AllOfSubCategoryCategory category) {
    this.category = category;
  }

  public SubCategory category2(Category category2) {
    this.category2 = category2;
    return this;
  }

  /**
   * Get category2
   * @return category2
  **/
  @ApiModelProperty(value = "")
  
    @Valid
    public Category getCategory2() {
    return category2;
  }

  public void setCategory2(Category category2) {
    this.category2 = category2;
  }

  public SubCategory pets(List<AllOfSubCategoryPetsItems> pets) {
    this.pets = pets;
    return this;
  }

  public SubCategory addPetsItem(AllOfSubCategoryPetsItems petsItem) {
    if (this.pets == null) {
      this.pets = new ArrayList<AllOfSubCategoryPetsItems>();
    }
    this.pets.add(petsItem);
    return this;
  }

  /**
   * Get pets
   * @return pets
  **/
  @ApiModelProperty(value = "")
  
    public List<AllOfSubCategoryPetsItems> getPets() {
    return pets;
  }

  public void setPets(List<AllOfSubCategoryPetsItems> pets) {
    this.pets = pets;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SubCategory subCategory = (SubCategory) o;
    return Objects.equals(this.category, subCategory.category) &&
        Objects.equals(this.category2, subCategory.category2) &&
        Objects.equals(this.pets, subCategory.pets);
  }

  @Override
  public int hashCode() {
    return Objects.hash(category, category2, pets);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SubCategory {\n");
    
    sb.append("    category: ").append(toIndentedString(category)).append("\n");
    sb.append("    category2: ").append(toIndentedString(category2)).append("\n");
    sb.append("    pets: ").append(toIndentedString(pets)).append("\n");
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
