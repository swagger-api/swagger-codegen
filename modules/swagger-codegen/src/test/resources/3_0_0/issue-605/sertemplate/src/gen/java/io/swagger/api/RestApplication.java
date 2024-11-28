package io.swagger.api;

import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Application;

import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;


@ApplicationPath("/")
public class RestApplication extends Application {

@Override
    public Set<Class<?>> getClasses() {
        return Stream.of(InventoryApi.class).collect(Collectors.toSet());
    }
}
