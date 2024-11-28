package io.swagger.codegen.v3.cli.cmd;

import ch.lambdaj.collection.LambdaIterable;
import io.swagger.codegen.v3.CodegenConfig;

import static ch.lambdaj.Lambda.on;
import static ch.lambdaj.collection.LambdaCollections.with;
import static java.util.ServiceLoader.load;

/**
 * User: lanwen Date: 24.03.15 Time: 20:25
 */
public class Langs implements Runnable {
    @Override
    public void run() {
        LambdaIterable<String> langs =
                with(load(CodegenConfig.class)).extract(on(CodegenConfig.class).getName());
        System.out.printf("Available languages: %s%n", langs);
    }
}
