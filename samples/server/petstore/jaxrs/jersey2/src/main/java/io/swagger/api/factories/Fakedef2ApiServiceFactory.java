package io.swagger.api.factories;

import io.swagger.api.Fakedef2ApiService;
import io.swagger.api.impl.Fakedef2ApiServiceImpl;


public class Fakedef2ApiServiceFactory {
    private final static Fakedef2ApiService service = new Fakedef2ApiServiceImpl();

    public static Fakedef2ApiService getFakedef2Api() {
        return service;
    }
}
