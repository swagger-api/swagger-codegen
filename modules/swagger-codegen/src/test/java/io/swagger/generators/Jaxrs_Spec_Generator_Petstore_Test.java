package io.swagger.generators;

import static org.junit.Assert.*;

import org.junit.Test;

import io.swagger.codegen.generators.JaxrsSpec_Generator_Petstore;

public class Jaxrs_Spec_Generator_Petstore_Test {


    @Test
    public void test() {
        assertTrue(JaxrsSpec_Generator_Petstore.generate());
    }

}
