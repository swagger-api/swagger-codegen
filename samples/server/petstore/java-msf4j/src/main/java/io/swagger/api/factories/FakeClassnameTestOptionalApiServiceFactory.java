package io.swagger.api.factories;

import io.swagger.api.FakeClassnameTestOptionalApiService;
import io.swagger.api.impl.FakeClassnameTestOptionalApiServiceImpl;

public class FakeClassnameTestOptionalApiServiceFactory {
    private final static FakeClassnameTestOptionalApiService service = new FakeClassnameTestOptionalApiServiceImpl();

    public static FakeClassnameTestOptionalApiService getFakeClassnameTestOptionalApi() {
        return service;
    }
}
