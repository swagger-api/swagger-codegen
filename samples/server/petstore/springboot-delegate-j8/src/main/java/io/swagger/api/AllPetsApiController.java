package io.swagger.api;

import org.springframework.web.bind.annotation.RestController;

@RestController
public class AllPetsApiController implements AllPetsApi {

    private final AllPetsApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public AllPetsApiController(AllPetsApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public AllPetsApiDelegate getDelegate() {
        return delegate;
    }
}
