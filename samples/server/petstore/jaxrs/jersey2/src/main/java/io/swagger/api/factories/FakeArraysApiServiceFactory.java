package io.swagger.api.factories;

import io.swagger.api.FakeArraysApiService;
import io.swagger.api.impl.FakeArraysApiServiceImpl;


public class FakeArraysApiServiceFactory {
    private final static FakeArraysApiService service = new FakeArraysApiServiceImpl();

    public static FakeArraysApiService getFakeArraysApi() {
        return service;
    }
}
