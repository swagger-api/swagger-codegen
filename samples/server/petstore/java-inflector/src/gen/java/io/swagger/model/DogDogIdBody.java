package io.swagger.model;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.v3.oas.annotations.media.Schema;




public class DogDogIdBody   {
  @JsonProperty("name")
  private String name = null;
  @JsonProperty("status")
  private String status = null;
  /**
   * Updated name of the dog
   **/
  public DogDogIdBody name(String name) {
    this.name = name;
    return this;
  }

  
  @Schema(description = "Updated name of the dog")
  @JsonProperty("name")
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  /**
   * Updated status of the dog
   **/
  public DogDogIdBody status(String status) {
    this.status = status;
    return this;
  }

  
  @Schema(description = "Updated status of the dog")
  @JsonProperty("status")
  public String getStatus() {
    return status;
  }
  public void setStatus(String status) {
    this.status = status;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    DogDogIdBody dogDogIdBody = (DogDogIdBody) o;
    return Objects.equals(name, dogDogIdBody.name) &&
        Objects.equals(status, dogDogIdBody.status);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, status);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class DogDogIdBody {\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    status: ").append(toIndentedString(status)).append("\n");
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
