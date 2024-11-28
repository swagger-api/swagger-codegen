package io.swagger.api;

import org.springframework.stereotype.Controller;
@Controller
public class ParrotApiController implements ParrotApi {

    private final ParrotApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public ParrotApiController(ParrotApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public ParrotApiDelegate getDelegate() {
        return delegate;
    }
}
