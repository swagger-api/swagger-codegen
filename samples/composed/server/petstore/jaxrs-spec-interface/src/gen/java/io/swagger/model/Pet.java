package io.swagger.model;

import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class Pet   {
  private @Valid Long id = null;
  private @Valid String name = null;
public enum StatusEnum {

    AVAILABLE(String.valueOf("available")), PENDING(String.valueOf("pending")), SOLD(String.valueOf("sold"));


    private String value;

    StatusEnum (String v) {
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
    public static StatusEnum fromValue(String v) {
        for (StatusEnum b : StatusEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}
  private @Valid StatusEnum status = null;
  private @Valid List<OneOfPetPartItems> part = new ArrayList<OneOfPetPartItems>();

  /**
   **/
  public Pet id(Long id) {
    this.id = id;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("id")

  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  /**
   **/
  public Pet name(String name) {
    this.name = name;
    return this;
  }

  
  @ApiModelProperty(example = "doggie", required = true, value = "")
  @JsonProperty("name")
  @NotNull

  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  /**
   * pet status in the store
   **/
  public Pet status(StatusEnum status) {
    this.status = status;
    return this;
  }

  
  @ApiModelProperty(value = "pet status in the store")
  @JsonProperty("status")

  public StatusEnum getStatus() {
    return status;
  }
  public void setStatus(StatusEnum status) {
    this.status = status;
  }

  /**
   **/
  public Pet part(List<OneOfPetPartItems> part) {
    this.part = part;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("part")

  public List<OneOfPetPartItems> getPart() {
    return part;
  }
  public void setPart(List<OneOfPetPartItems> part) {
    this.part = part;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Pet pet = (Pet) o;
    return Objects.equals(id, pet.id) &&
        Objects.equals(name, pet.name) &&
        Objects.equals(status, pet.status) &&
        Objects.equals(part, pet.part);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, name, status, part);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Pet {\n");
    
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    status: ").append(toIndentedString(status)).append("\n");
    sb.append("    part: ").append(toIndentedString(part)).append("\n");
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
