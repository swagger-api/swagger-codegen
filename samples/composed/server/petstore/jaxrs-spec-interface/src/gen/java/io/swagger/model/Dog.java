package io.swagger.model;

import io.swagger.model.Pet;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class Dog extends Pet implements OneOfAllPetsResponseItems, OneOfPetPartItems, OneOfPup  {
  private @Valid Boolean bark = null;
public enum BreedEnum {

    DINGO(String.valueOf("Dingo")), HUSKY(String.valueOf("Husky")), RETRIEVER(String.valueOf("Retriever")), SHEPHERD(String.valueOf("Shepherd"));


    private String value;

    BreedEnum (String v) {
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
    public static BreedEnum fromValue(String v) {
        for (BreedEnum b : BreedEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}
  private @Valid BreedEnum breed = null;

  /**
   **/
  public Dog bark(Boolean bark) {
    this.bark = bark;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("bark")

  public Boolean isBark() {
    return bark;
  }
  public void setBark(Boolean bark) {
    this.bark = bark;
  }

  /**
   **/
  public Dog breed(BreedEnum breed) {
    this.breed = breed;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("breed")

  public BreedEnum getBreed() {
    return breed;
  }
  public void setBreed(BreedEnum breed) {
    this.breed = breed;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Dog dog = (Dog) o;
    return Objects.equals(bark, dog.bark) &&
        Objects.equals(breed, dog.breed);
  }

  @Override
  public int hashCode() {
    return Objects.hash(bark, breed);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Dog {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    bark: ").append(toIndentedString(bark)).append("\n");
    sb.append("    breed: ").append(toIndentedString(breed)).append("\n");
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
