package io.swagger.api.factories;

import io.swagger.api.DefaultApiService;
import io.swagger.api.impl.DefaultApiServiceImpl;

public class DefaultApiServiceFactory {
    private final static DefaultApiService service = new DefaultApiServiceImpl();

    public static DefaultApiService getDefaultApi() {
        return service;
    }
}
