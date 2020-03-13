package io.swagger.model;

import io.swagger.model.Category;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;

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

public class SubCategory   {
  
  @Schema(description = "")
  private AllOfSubCategoryCategory category = null;
  
  @Schema(description = "")
  private Category category2 = null;
  
  @Schema(description = "")
  private List<AllOfSubCategoryPetsItems> pets = null;
 /**
   * Get category
   * @return category
  **/
  @JsonProperty("category")
  public AllOfSubCategoryCategory getCategory() {
    return category;
  }

  public void setCategory(AllOfSubCategoryCategory category) {
    this.category = category;
  }

  public SubCategory category(AllOfSubCategoryCategory category) {
    this.category = category;
    return this;
  }

 /**
   * Get category2
   * @return category2
  **/
  @JsonProperty("category2")
  public Category getCategory2() {
    return category2;
  }

  public void setCategory2(Category category2) {
    this.category2 = category2;
  }

  public SubCategory category2(Category category2) {
    this.category2 = category2;
    return this;
  }

 /**
   * Get pets
   * @return pets
  **/
  @JsonProperty("pets")
  public List<AllOfSubCategoryPetsItems> getPets() {
    return pets;
  }

  public void setPets(List<AllOfSubCategoryPetsItems> pets) {
    this.pets = pets;
  }

  public SubCategory pets(List<AllOfSubCategoryPetsItems> pets) {
    this.pets = pets;
    return this;
  }

  public SubCategory addPetsItem(AllOfSubCategoryPetsItems petsItem) {
    this.pets.add(petsItem);
    return this;
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
  private static String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}
