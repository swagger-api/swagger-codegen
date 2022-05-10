package io.swagger.api.factories;

import io.swagger.api.AllPetsApiService;
import io.swagger.api.impl.AllPetsApiServiceImpl;

public class AllPetsApiServiceFactory {
    private final static AllPetsApiService service = new AllPetsApiServiceImpl();

    public static AllPetsApiService getAllPetsApi() {
        return service;
    }
}
