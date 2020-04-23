package io.swagger.api;

import io.swagger.model.SinglePetResponse;
import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * A delegate to be called by the {@link RandomPetApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
public interface RandomPetApiDelegate {

    /**
     * @see RandomPetApi#getRandomPet
     */
    ResponseEntity<SinglePetResponse> getRandomPet();

}
