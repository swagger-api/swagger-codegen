package io.swagger.api;

import org.springframework.stereotype.Controller;

import javax.validation.constraints.*;
import javax.validation.Valid;

@Controller
public class ApiApiController implements ApiApi {
    private final ObjectMapper objectMapper;

    public ApiApiController(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

}
