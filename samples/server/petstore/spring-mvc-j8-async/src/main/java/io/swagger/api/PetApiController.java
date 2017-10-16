package io.swagger.api;

import org.springframework.stereotype.Controller;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import javax.validation.constraints.*;
import javax.validation.Valid;
@javax.annotation.Generated(value = "io.swagger.codegen.languages.SpringCodegen", date = "2017-10-16T17:46:17.828+02:00")

@Controller
public class PetApiController implements PetApi {
    private final ObjectMapper objectMapper;

    public PetApiController(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

}
