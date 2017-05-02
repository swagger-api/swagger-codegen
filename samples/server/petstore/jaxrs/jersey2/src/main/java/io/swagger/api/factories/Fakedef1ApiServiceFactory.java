package io.swagger.api.factories;

import io.swagger.api.Fakedef1ApiService;
import io.swagger.api.impl.Fakedef1ApiServiceImpl;


public class Fakedef1ApiServiceFactory {
    private final static Fakedef1ApiService service = new Fakedef1ApiServiceImpl();

    public static Fakedef1ApiService getFakedef1Api() {
        return service;
    }
}
