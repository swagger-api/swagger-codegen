package io.swagger.api;

import java.util.concurrent.CompletableFuture;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import io.swagger.model.Client;


@Api(value = "fake_classname_test", description = "the fake_classname_test API")
public interface FakeClassnameTestApi {

    @ApiOperation(value = "To test class name in snake case", notes = "", response = Client.class, tags={ "fake_classname_tags 123#$%^", })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    @RequestMapping(value = "/fake_classname_test",
        produces = { "application/json" }, 
        consumes = { "application/json" },
        method = RequestMethod.PATCH)
    default CompletableFuture<ResponseEntity<Client>> testClassname(@ApiParam(value = "client model" ,required=true ) @RequestBody Client body) {
        // do some magic!
        return CompletableFuture.completedFuture(new ResponseEntity<Client>(HttpStatus.OK));
    }

}
