package io.swagger.api.factories;

import io.swagger.api.AnimalApiService;
import io.swagger.api.impl.AnimalApiServiceImpl;

public class AnimalApiServiceFactory {
    private final static AnimalApiService service = new AnimalApiServiceImpl();

    public static AnimalApiService getAnimalApi() {
        return service;
    }
}
