package io.swagger.api.factories;

import io.swagger.api.ParrotApiService;
import io.swagger.api.impl.ParrotApiServiceImpl;

public class ParrotApiServiceFactory {
    private final static ParrotApiService service = new ParrotApiServiceImpl();

    public static ParrotApiService getParrotApi() {
        return service;
    }
}
