package io.swagger.model;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;   //
/**
* OneOfSinglePetResponsePet
*/
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
  @JsonSubTypes({
  @JsonSubTypes.Type(value = Dog.class, name = "Dog"),
  @JsonSubTypes.Type(value = Cat.class, name = "Cat")
})
public interface OneOfSinglePetResponsePet {
}
