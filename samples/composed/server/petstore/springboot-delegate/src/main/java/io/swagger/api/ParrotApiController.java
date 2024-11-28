package io.swagger.api;

import io.swagger.model.Body1;
import io.swagger.model.Body2;
import io.swagger.model.InlineResponse200;
import io.swagger.model.InlineResponse2001;
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
public class ParrotApiController implements ParrotApi {

    private final ParrotApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public ParrotApiController(ParrotApiDelegate delegate) {
        this.delegate = delegate;
    }
    public ResponseEntity<InlineResponse2001> addParrot(@ApiParam(value = ""  )  @Valid @RequestBody Body2 body
) {
        return delegate.addParrot(body);
    }

    public ResponseEntity<List<Object>> getParrots() {
        return delegate.getParrots();
    }

    public ResponseEntity<InlineResponse200> updateParrots(@ApiParam(value = ""  )  @Valid @RequestBody Body1 body
) {
        return delegate.updateParrots(body);
    }

}
