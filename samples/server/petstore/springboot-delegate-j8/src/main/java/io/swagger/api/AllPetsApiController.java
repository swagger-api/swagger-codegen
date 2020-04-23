package io.swagger.api;

import org.springframework.stereotype.Controller;
@Controller
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
