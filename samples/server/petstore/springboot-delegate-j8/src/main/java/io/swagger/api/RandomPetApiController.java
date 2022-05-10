package io.swagger.api;

import org.springframework.web.bind.annotation.RestController;

@RestController
public class RandomPetApiController implements RandomPetApi {

    private final RandomPetApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public RandomPetApiController(RandomPetApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public RandomPetApiDelegate getDelegate() {
        return delegate;
    }
}
