package io.swagger.codegen.languages;

import io.swagger.models.Swagger;
import io.swagger.models.Info;
import org.testng.annotations.Test;

public class SwaggerGeneratorTest {

    @Test
    public void testProcessSwaggerWithPathTraversal() {
        SwaggerGenerator generator = new SwaggerGenerator();
        generator.setOutputFile("../../../etc/passwd");

        Swagger swagger = new Swagger();
        swagger.setInfo(new Info().title("Test API").version("1.0.0"));

        generator.processSwagger(swagger);
    }
}
