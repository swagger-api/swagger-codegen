package io.swagger.model;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;




public class DogAllOf2   {
  @JsonProperty("bark")
  private Boolean bark = null;
  /**
   * Gets or Sets breed
   */
  public enum BreedEnum {
    DINGO("Dingo"),
    
    HUSKY("Husky"),
    
    RETRIEVER("Retriever"),
    
    SHEPHERD("Shepherd");

    private String value;

    BreedEnum(String value) {
      this.value = value;
    }

    @Override
    @JsonValue
    public String toString() {
      return String.valueOf(value);
    }

    @JsonCreator
    public static BreedEnum fromValue(String text) {
      for (BreedEnum b : BreedEnum.values()) {
        if (String.valueOf(b.value).equals(text)) {
          return b;
        }
      }
      return null;
    }
  }
  @JsonProperty("breed")
  private BreedEnum breed = null;
  /**
   **/
  public DogAllOf2 bark(Boolean bark) {
    this.bark = bark;
    return this;
  }

  
  @Schema(description = "")
  @JsonProperty("bark")
  public Boolean isBark() {
    return bark;
  }
  public void setBark(Boolean bark) {
    this.bark = bark;
  }

  /**
   **/
  public DogAllOf2 breed(BreedEnum breed) {
    this.breed = breed;
    return this;
  }

  
  @Schema(description = "")
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
    DogAllOf2 dogAllOf2 = (DogAllOf2) o;
    return Objects.equals(bark, dogAllOf2.bark) &&
        Objects.equals(breed, dogAllOf2.breed);
  }

  @Override
  public int hashCode() {
    return Objects.hash(bark, breed);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class DogAllOf2 {\n");
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
