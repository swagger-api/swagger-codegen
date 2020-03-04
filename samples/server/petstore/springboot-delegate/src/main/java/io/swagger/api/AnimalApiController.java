package io.swagger.api;

import io.swagger.model.Animal;
import io.swagger.annotations.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.constraints.*;
import javax.validation.Valid;
import java.util.List;
import java.util.Map;
@Controller
public class AnimalApiController implements AnimalApi {

    private final AnimalApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public AnimalApiController(AnimalApiDelegate delegate) {
        this.delegate = delegate;
    }
    public ResponseEntity<Void> addAnimal(@ApiParam(value = "Animal object that needs to be added to the store" ,required=true )  @Valid @RequestBody Animal body
) {
        return delegate.addAnimal(body);
    }

    public ResponseEntity<Void> deleteAnimal(@ApiParam(value = "Animal id to delete",required=true) @PathVariable("animalId") Long animalId
,@ApiParam(value = "" ) @RequestHeader(value="api_key", required=false) String apiKey
) {
        return delegate.deleteAnimal(animalId, apiKey);
    }

    public ResponseEntity<Animal> getAnimalById(@ApiParam(value = "ID of pet to return",required=true) @PathVariable("animalId") Long animalId
) {
        return delegate.getAnimalById(animalId);
    }

    public ResponseEntity<Void> updateAnimal(@ApiParam(value = "Animal object that needs to be added." ,required=true )  @Valid @RequestBody Animal body
) {
        return delegate.updateAnimal(body);
    }

    public ResponseEntity<Void> updateAnimalWithForm(@ApiParam(value = "ID of animal that needs to be updated",required=true) @PathVariable("animalId") Long animalId
,@ApiParam(value = "") @RequestParam(value="name", required=false)  String name
,@ApiParam(value = "") @RequestParam(value="status", required=false)  String status
) {
        return delegate.updateAnimalWithForm(animalId, name, status);
    }

}
