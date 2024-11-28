package io.swagger.api;

import org.springframework.web.bind.annotation.RestController;

@RestController
public class DogApiController implements DogApi {

    private final DogApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public DogApiController(DogApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public DogApiDelegate getDelegate() {
        return delegate;
    }
}
