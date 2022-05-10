package io.swagger.api.factories;

import io.swagger.api.DogApiService;
import io.swagger.api.impl.DogApiServiceImpl;

public class DogApiServiceFactory {
    private final static DogApiService service = new DogApiServiceImpl();

    public static DogApiService getDogApi() {
        return service;
    }
}
