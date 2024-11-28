package io.swagger.api;

import io.swagger.model.Animal;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * A delegate to be called by the {@link AnimalApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
public interface AnimalApiDelegate {

    /**
     * @see AnimalApi#addAnimal
     */
    ResponseEntity<Void> addAnimal( Animal  body);

    /**
     * @see AnimalApi#deleteAnimal
     */
    ResponseEntity<Void> deleteAnimal( Long  animalId,
         String  apiKey);

    /**
     * @see AnimalApi#getAnimalById
     */
    ResponseEntity<Animal> getAnimalById( Long  animalId);

    /**
     * @see AnimalApi#updateAnimal
     */
    ResponseEntity<Void> updateAnimal( Animal  body);

    /**
     * @see AnimalApi#updateAnimalWithForm
     */
    ResponseEntity<Void> updateAnimalWithForm( Long  animalId,
         String  name,
         String  status);

}
