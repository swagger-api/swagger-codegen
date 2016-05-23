package io.swagger.generator;

import org.testng.annotations.Test;

import io.swagger.generator.model.GeneratorInput;

@SuppressWarnings("static-method")
public class GeneratorInputTest {

    @Test(description = "write an object")
    public void writeObjectTest() {
        final GeneratorInput generatorInput = new GeneratorInput();
        generatorInput.setSwaggerUrl("http://petstore.swagger.io/v2/swagger.json");
    }
}
