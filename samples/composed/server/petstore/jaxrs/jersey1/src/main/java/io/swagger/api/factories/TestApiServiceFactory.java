package io.swagger.api.factories;

import io.swagger.api.TestApiService;
import io.swagger.api.impl.TestApiServiceImpl;

public class TestApiServiceFactory {
    private final static TestApiService service = new TestApiServiceImpl();

    public static TestApiService getTestApi() {
        return service;
    }
}
