package io.swagger.api.factories;

import io.swagger.api.Fakedef3ApiService;
import io.swagger.api.impl.Fakedef3ApiServiceImpl;


public class Fakedef3ApiServiceFactory {
    private final static Fakedef3ApiService service = new Fakedef3ApiServiceImpl();

    public static Fakedef3ApiService getFakedef3Api() {
        return service;
    }
}
