package io.swagger.api;

import io.swagger.model.Client;

import io.swagger.annotations.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Optional;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import javax.servlet.http.HttpServletRequest;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Controller
public class AnotherFakeApiController implements AnotherFakeApi {

    private static final Logger log = LoggerFactory.getLogger(AnotherFakeApiController.class);

    private final ObjectMapper objectMapper;

    @org.springframework.beans.factory.annotation.Autowired
    private HttpServletRequest request;

    @org.springframework.beans.factory.annotation.Autowired
    public AnotherFakeApiController(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    public ResponseEntity<Client> testSpecialTags(@ApiParam(value = "client model" ,required=true )  @Valid @RequestBody Client body) {
        // do some magic!
        String accept = request.getHeader("Accept");
        if (accept != null && accept.contains("application/json")) {
            try {
                return new ResponseEntity<Client>(objectMapper.readValue("{  \"client\" : \"client\"}", Client.class), HttpStatus.NOT_IMPLEMENTED);
            } catch (IOException e) {
                log.error("Couldn't serialize response for content type application/json", e);
                return new ResponseEntity<Client>(HttpStatus.INTERNAL_SERVER_ERROR);
            }
        }

        return new ResponseEntity<Client>(HttpStatus.OK);
    }

}
