package io.swagger.api.factories;

import io.swagger.api.PetApiService;
import io.swagger.api.impl.PetApiServiceImpl;


public class PetApiServiceFactory {
    private final static PetApiService service = new PetApiServiceImpl();

    private PetApiServiceFactory(){
    // private constructor because singleton
    }

    public static PetApiService getPetApi() {
        return service;
    }
}
