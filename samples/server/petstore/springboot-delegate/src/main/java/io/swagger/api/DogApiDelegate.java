package io.swagger.api;

import io.swagger.model.Dog;
import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * A delegate to be called by the {@link DogApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
public interface DogApiDelegate {

    /**
     * @see DogApi#addDog
     */
    ResponseEntity<Void> addDog( Dog  body);

    /**
     * @see DogApi#deleteDog
     */
    ResponseEntity<Void> deleteDog( Long  dogId,
         String  apiKey);

    /**
     * @see DogApi#getDogById
     */
    ResponseEntity<Dog> getDogById( Long  dogId);

    /**
     * @see DogApi#updateDog
     */
    ResponseEntity<Void> updateDog( Dog  body);

    /**
     * @see DogApi#updateDogWithForm
     */
    ResponseEntity<Void> updateDogWithForm( Long  dogId,
         String  name,
         String  status);

}
