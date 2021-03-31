package io.swagger.api;

import org.springframework.web.bind.annotation.RestController;

@RestController
public class AnotherFakeApiController implements AnotherFakeApi {

    private final AnotherFakeApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public AnotherFakeApiController(AnotherFakeApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public AnotherFakeApiDelegate getDelegate() {
        return delegate;
    }
}
