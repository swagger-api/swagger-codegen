package io.swagger.codegen.javaSparkjava;

import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.JavaSparkjavaServerCodegen;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.StringProperty;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class JavaSparkjavaServerCodegenModelTest {

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("name", new StringProperty())
                .required("id")
                .required("name");
        final DefaultCodegen codegen = new JavaSparkjavaServerCodegen();

        // TODO: Complete this test.
//        Assert.fail("Not implemented.");
    }

}

