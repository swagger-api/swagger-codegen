package io.swagger.api;

import io.swagger.model.AllPetsResponse;
import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * A delegate to be called by the {@link AllPetsApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
public interface AllPetsApiDelegate {

    /**
     * @see AllPetsApi#getAllPets
     */
    ResponseEntity<AllPetsResponse> getAllPets();

}
