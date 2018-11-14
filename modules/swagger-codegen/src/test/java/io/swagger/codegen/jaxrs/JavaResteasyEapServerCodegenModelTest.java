package io.swagger.codegen.jaxrs;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.languages.JavaJerseyServerCodegen;
import io.swagger.codegen.languages.JavaResteasyEapServerCodegen;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.util.Json;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

public class JavaResteasyEapServerCodegenModelTest {
    @Test(description = "convert a simple java model with java8 types")
    public void mapModelTest() {
        final Model model = new ModelImpl()
                .description("A model with a map")
                .property("map", new MapProperty());

        final JavaResteasyEapServerCodegen codegen = new JavaResteasyEapServerCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Json.prettyPrint(cm);
        assertEquals(cm.vars.get(0).baseType, "Map");
        assertTrue(cm.imports.contains("HashMap"));
    }
}
