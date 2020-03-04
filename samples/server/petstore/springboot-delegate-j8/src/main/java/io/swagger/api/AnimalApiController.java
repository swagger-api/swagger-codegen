package io.swagger.api;

import org.springframework.stereotype.Controller;
@Controller
public class AnimalApiController implements AnimalApi {

    private final AnimalApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public AnimalApiController(AnimalApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public AnimalApiDelegate getDelegate() {
        return delegate;
    }
}
