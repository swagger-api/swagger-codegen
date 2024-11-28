package io.swagger.api.factories;

import io.swagger.api.RandomPetApiService;
import io.swagger.api.impl.RandomPetApiServiceImpl;

public class RandomPetApiServiceFactory {
    private final static RandomPetApiService service = new RandomPetApiServiceImpl();

    public static RandomPetApiService getRandomPetApi() {
        return service;
    }
}
