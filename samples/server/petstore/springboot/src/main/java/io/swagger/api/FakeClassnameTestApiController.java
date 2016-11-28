package io.swagger.api;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;

import io.swagger.model.Client;



@Controller
public class FakeClassnameTestApiController implements FakeClassnameTestApi {

    public ResponseEntity<Client> testClassname(@ApiParam(value = "client model" ,required=true ) @RequestBody Client body) {
        // do some magic!
        return new ResponseEntity<Client>(HttpStatus.OK);
    }

}
