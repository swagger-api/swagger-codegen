package io.swagger.api;

import io.swagger.model.User;
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
 * A delegate to be called by the {@link UserApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
public interface UserApiDelegate {

    /**
     * @see UserApi#createUser
     */
    ResponseEntity<Void> createUser( User  body);

    /**
     * @see UserApi#createUsersWithArrayInput
     */
    ResponseEntity<Void> createUsersWithArrayInput( List<User>  body);

    /**
     * @see UserApi#createUsersWithListInput
     */
    ResponseEntity<Void> createUsersWithListInput( List<User>  body);

    /**
     * @see UserApi#deleteUser
     */
    ResponseEntity<Void> deleteUser( String  username);

    /**
     * @see UserApi#getUserByName
     */
    ResponseEntity<User> getUserByName( String  username);

    /**
     * @see UserApi#loginUser
     */
    ResponseEntity<String> loginUser( String  username,
         String  password);

    /**
     * @see UserApi#logoutUser
     */
    ResponseEntity<Void> logoutUser();

    /**
     * @see UserApi#updateUser
     */
    ResponseEntity<Void> updateUser( String  username,
         User  body);

}
