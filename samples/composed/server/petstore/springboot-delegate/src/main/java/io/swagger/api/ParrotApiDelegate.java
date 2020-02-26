package io.swagger.api;

import io.swagger.model.Body1;
import io.swagger.model.Body2;
import io.swagger.model.InlineResponse200;
import io.swagger.model.InlineResponse2001;
import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * A delegate to be called by the {@link ParrotApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
public interface ParrotApiDelegate {

    /**
     * @see ParrotApi#addParrot
     */
    ResponseEntity<InlineResponse2001> addParrot( Body2  body);

    /**
     * @see ParrotApi#getParrots
     */
    ResponseEntity<List<Object>> getParrots();

    /**
     * @see ParrotApi#updateParrots
     */
    ResponseEntity<InlineResponse200> updateParrots( Body1  body);

}
