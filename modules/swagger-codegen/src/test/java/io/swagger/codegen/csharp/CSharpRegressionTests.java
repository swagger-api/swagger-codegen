package io.swagger.codegen.csharp;

import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenResponse;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.CSharpClientCodegen;
import io.swagger.models.ModelImpl;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.models.parameters.BodyParameter;
import io.swagger.models.properties.RefProperty;
import io.swagger.parser.SwaggerParser;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class CSharpRegressionTests {

    @Test(description = "convert parameters from additionalProperties mapped $ref")
    public void issue5132() {

        final Swagger swagger = new SwaggerParser().read("src/test/resources/2_0/5132.json");
        final DefaultCodegen codegen = new CSharpClientCodegen();
        final Operation operation = swagger.getPath("/Resources").getGet();

        // First we verify the parser is exposing ResourceParameters in the expected way
        final BodyParameter parameter = (BodyParameter) operation.getParameters().get(0);
        assertEquals(parameter.getIn(), "body");
        assertEquals(parameter.getDescription(), "ResourceParameters object that needs to be added");
        assertEquals(parameter.getRequired(), true);

        final ModelImpl modelImpl = (ModelImpl)parameter.getSchema();
        assertEquals(modelImpl.getType(), "object");

        final RefProperty refProperty = (RefProperty)modelImpl.getAdditionalProperties();
        assertEquals(refProperty.getSimpleRef(), "ResourceParameters");

        final CodegenOperation co = codegen.fromOperation(
                "/Resources",
                "GET",
                operation,
                swagger.getDefinitions(),
                swagger
        );

        final CodegenResponse response = co.responses.get(0);
        assertTrue(response.isMapContainer);

        // Then we verify CodeGen is exposing the parameter as expected.
        // Here we expect a body parameter of type ResourceParameters { firstName, lastName, skip, top }
        assertTrue(co.getHasBodyParam(), "Expected body param");
        final CodegenParameter cp = co.bodyParam;

        assertTrue(cp.isBodyParam, "Failed to expose isBodyParam");
        assertEquals(cp.paramName, "resourceParameters");
        assertNotEquals(cp.dataType, "Object", "Regression of issue 5132 found, see https://github.com/swagger-api/swagger-codegen/issues/5132.");
        assertEquals(cp.dataType, "ResourceParameters", "Possible regression of issue 5132 found, see https://github.com/swagger-api/swagger-codegen/issues/5132.");
    }
}
