package io.swagger.api;

import org.springframework.web.bind.annotation.RestController;

@RestController
public class FakeApiController implements FakeApi {

    private final FakeApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public FakeApiController(FakeApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public FakeApiDelegate getDelegate() {
        return delegate;
    }
}
