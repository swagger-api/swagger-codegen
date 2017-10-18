package io.swagger.api;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.stereotype.Controller;
@javax.annotation.Generated(value = "io.swagger.codegen.languages.SpringCodegen", date = "2017-10-18T14:54:52.329+03:00")

@Controller
public class UserApiController implements UserApi {
    private final ObjectMapper objectMapper;

    public UserApiController(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }
}
