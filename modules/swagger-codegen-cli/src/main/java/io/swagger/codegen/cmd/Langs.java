package io.swagger.codegen.cmd;

import io.airlift.airline.Command;
import io.swagger.codegen.CodegenConfig;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import static java.util.ServiceLoader.load;

/**
 * User: lanwen Date: 24.03.15 Time: 20:25
 */
@Command(name = "langs", description = "Shows available langs")
public class Langs implements Runnable {
    @Override
    public void run() {
        List<String> langs = StreamSupport.stream(load(CodegenConfig.class).spliterator(), false)
                .map(CodegenConfig::getName)
                .collect(Collectors.toList());
        System.out.printf("Available languages: %s%n", langs);
    }
}
