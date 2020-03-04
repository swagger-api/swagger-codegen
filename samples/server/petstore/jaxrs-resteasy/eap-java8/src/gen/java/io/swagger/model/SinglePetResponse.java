package io.swagger.model;

import java.util.Objects;
import java.util.ArrayList;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


public class SinglePetResponse   {
  private OneOfSinglePetResponsePet pet = null;

  /**
   **/
  
  @Schema(description = "")
  @JsonProperty("pet")
  public OneOfSinglePetResponsePet getPet() {
    return pet;
  }
  public void setPet(OneOfSinglePetResponsePet pet) {
    this.pet = pet;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SinglePetResponse singlePetResponse = (SinglePetResponse) o;
    return Objects.equals(pet, singlePetResponse.pet);
  }

  @Override
  public int hashCode() {
    return Objects.hash(pet);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SinglePetResponse {\n");
    
    sb.append("    pet: ").append(toIndentedString(pet)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}
