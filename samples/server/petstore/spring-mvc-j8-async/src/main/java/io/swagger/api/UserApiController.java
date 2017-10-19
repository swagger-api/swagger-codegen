package io.swagger.api;

import org.springframework.stereotype.Controller;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import javax.validation.constraints.*;
import javax.validation.Valid;
@javax.annotation.Generated(value = "io.swagger.codegen.languages.SpringCodegen", date = "2017-10-19T13:23:27.059+03:00")

@Controller
public class UserApiController implements UserApi {
    private final ObjectMapper objectMapper;

    public UserApiController(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }
}
