package io.swagger.api;

import io.swagger.model.Dog;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.constraints.*;
import javax.validation.Valid;
import java.util.List;
import java.util.Map;

@RestController
public class DogApiController implements DogApi {

    private final DogApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public DogApiController(DogApiDelegate delegate) {
        this.delegate = delegate;
    }
    public ResponseEntity<Void> addDog(@Parameter(in = ParameterIn.DEFAULT, description = "Dog object that needs to be added to the store", required=true, schema=@Schema()) @Valid @RequestBody Dog body) {
        return delegate.addDog(body);
    }

    public ResponseEntity<Void> deleteDog(@Parameter(in = ParameterIn.PATH, description = "Dog id to delete", required=true, schema=@Schema()) @PathVariable("dogId") Long dogId,@Parameter(in = ParameterIn.HEADER, description = "" ,schema=@Schema()) @RequestHeader(value="api_key", required=false) String apiKey) {
        return delegate.deleteDog(dogId, apiKey);
    }

    public ResponseEntity<Dog> getDogById(@Parameter(in = ParameterIn.PATH, description = "ID of dog to return", required=true, schema=@Schema()) @PathVariable("dogId") Long dogId) {
        return delegate.getDogById(dogId);
    }

    public ResponseEntity<Void> updateDog(@Parameter(in = ParameterIn.DEFAULT, description = "Dog object that needs to be added.", required=true, schema=@Schema()) @Valid @RequestBody Dog body) {
        return delegate.updateDog(body);
    }

    public ResponseEntity<Void> updateDogWithForm(@Parameter(in = ParameterIn.PATH, description = "ID of dog that needs to be updated", required=true, schema=@Schema()) @PathVariable("dogId") Long dogId,@Parameter(in = ParameterIn.DEFAULT, description = "",schema=@Schema()) @RequestParam(value="name", required=false)  String name,@Parameter(in = ParameterIn.DEFAULT, description = "",schema=@Schema()) @RequestParam(value="status", required=false)  String status) {
        return delegate.updateDogWithForm(dogId, name, status);
    }

}
