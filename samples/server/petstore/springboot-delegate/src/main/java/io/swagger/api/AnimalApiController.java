package io.swagger.api;

import io.swagger.model.Animal;
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
public class AnimalApiController implements AnimalApi {

    private final AnimalApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public AnimalApiController(AnimalApiDelegate delegate) {
        this.delegate = delegate;
    }
    public ResponseEntity<Void> addAnimal(@Parameter(in = ParameterIn.DEFAULT, description = "Animal object that needs to be added to the store", required=true, schema=@Schema()) @Valid @RequestBody Animal body) {
        return delegate.addAnimal(body);
    }

    public ResponseEntity<Void> deleteAnimal(@Parameter(in = ParameterIn.PATH, description = "Animal id to delete", required=true, schema=@Schema()) @PathVariable("animalId") Long animalId,@Parameter(in = ParameterIn.HEADER, description = "" ,schema=@Schema()) @RequestHeader(value="api_key", required=false) String apiKey) {
        return delegate.deleteAnimal(animalId, apiKey);
    }

    public ResponseEntity<Animal> getAnimalById(@Parameter(in = ParameterIn.PATH, description = "ID of pet to return", required=true, schema=@Schema()) @PathVariable("animalId") Long animalId) {
        return delegate.getAnimalById(animalId);
    }

    public ResponseEntity<Void> updateAnimal(@Parameter(in = ParameterIn.DEFAULT, description = "Animal object that needs to be added.", required=true, schema=@Schema()) @Valid @RequestBody Animal body) {
        return delegate.updateAnimal(body);
    }

    public ResponseEntity<Void> updateAnimalWithForm(@Parameter(in = ParameterIn.PATH, description = "ID of animal that needs to be updated", required=true, schema=@Schema()) @PathVariable("animalId") Long animalId,@Parameter(in = ParameterIn.DEFAULT, description = "",schema=@Schema()) @RequestParam(value="name", required=false)  String name,@Parameter(in = ParameterIn.DEFAULT, description = "",schema=@Schema()) @RequestParam(value="status", required=false)  String status) {
        return delegate.updateAnimalWithForm(animalId, name, status);
    }

}
