package io.swagger.api.factories;

import io.swagger.api.FakeApiService;
import io.swagger.api.impl.FakeApiServiceImpl;


public class FakeApiServiceFactory {
    private final static FakeApiService service = new FakeApiServiceImpl();

    private FakeApiServiceFactory(){
    // private constructor because singleton
    }

    public static FakeApiService getFakeApi() {
        return service;
    }
}
