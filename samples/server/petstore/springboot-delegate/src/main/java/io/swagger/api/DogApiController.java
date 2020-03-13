package io.swagger.api;

import io.swagger.model.Dog;
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
public class DogApiController implements DogApi {

    private final DogApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public DogApiController(DogApiDelegate delegate) {
        this.delegate = delegate;
    }
    public ResponseEntity<Void> addDog(@ApiParam(value = "Dog object that needs to be added to the store" ,required=true )  @Valid @RequestBody Dog body
) {
        return delegate.addDog(body);
    }

    public ResponseEntity<Void> deleteDog(@ApiParam(value = "Dog id to delete",required=true) @PathVariable("dogId") Long dogId
,@ApiParam(value = "" ) @RequestHeader(value="api_key", required=false) String apiKey
) {
        return delegate.deleteDog(dogId, apiKey);
    }

    public ResponseEntity<Dog> getDogById(@ApiParam(value = "ID of dog to return",required=true) @PathVariable("dogId") Long dogId
) {
        return delegate.getDogById(dogId);
    }

    public ResponseEntity<Void> updateDog(@ApiParam(value = "Dog object that needs to be added." ,required=true )  @Valid @RequestBody Dog body
) {
        return delegate.updateDog(body);
    }

    public ResponseEntity<Void> updateDogWithForm(@ApiParam(value = "ID of dog that needs to be updated",required=true) @PathVariable("dogId") Long dogId
,@ApiParam(value = "") @RequestParam(value="name", required=false)  String name
,@ApiParam(value = "") @RequestParam(value="status", required=false)  String status
) {
        return delegate.updateDogWithForm(dogId, name, status);
    }

}
