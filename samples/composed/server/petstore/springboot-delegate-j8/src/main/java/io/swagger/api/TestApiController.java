package io.swagger.api;

import org.springframework.stereotype.Controller;
@Controller
public class TestApiController implements TestApi {

    private final TestApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public TestApiController(TestApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public TestApiDelegate getDelegate() {
        return delegate;
    }
}
