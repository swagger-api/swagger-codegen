package io.swagger.api;

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
public class TestApiController implements TestApi {

    private final TestApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public TestApiController(TestApiDelegate delegate) {
        this.delegate = delegate;
    }
    public ResponseEntity<List<String>> testMethod() {
        return delegate.testMethod();
    }

}
