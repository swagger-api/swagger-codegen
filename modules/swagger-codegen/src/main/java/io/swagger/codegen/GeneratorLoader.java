package io.swagger.codegen;

import java.util.ServiceLoader;

public class GeneratorLoader {

    public static Iterable<Generator> load() {
        return ServiceLoader.load(Generator.class);
    }
}
