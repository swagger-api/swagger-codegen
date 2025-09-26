package io.swagger.codegen.languages;

import io.swagger.models.Swagger;
import io.swagger.models.Info;
import org.testng.Assert;
import org.testng.annotations.Test;

public class SwaggerYamlGeneratorTest {

    @Test
    public void testProcessSwaggerWithPathTraversal() {
        SwaggerYamlGenerator generator = new SwaggerYamlGenerator();

        generator.setOutputFile("../../../etc/passwd");

        Swagger swagger = new Swagger();
        swagger.setInfo(new Info().title("Test API").version("1.0.0"));

        try {
            generator.processSwagger(swagger);
        } catch (Exception e) {
            Assert.fail("processSwagger should handle SecurityException gracefully: " + e.getMessage());
        }
    }
}
