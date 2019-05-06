package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class AbstractCppCodegenTest {

    private class FakeCppCodegen extends AbstractCppCodegen {
        @Override
        public CodegenType getTag() {
            return null;
        }

        @Override
        public String getName() {
            return null;
        }

        @Override
        public String getHelp() {
            return null;
        }
    }

    @DataProvider(name = "typeNames")
    public static Object[][] typeNames() {
        return new Object[][] {
                {"sample", "Sample"},
                {"sample_name", "SampleName"},
                {"sample__name", "SampleName"},
                {"sample.name.foo", "SampleNameFoo"},
                {"_sample", "Sample"},
                {"__sample", "Sample"},
                {"Sample", "Sample"},
                {"SampleName", "SampleName"},
                {"123SampleABC", "Type123SampleABC"},
                {"1", "Type1"},
        };
    }

    @Test(dataProvider = "typeNames", description = "correct model name conversion")
    public void modelNameTest(String name, String expectedName) {
        final DefaultCodegen codegen = new FakeCppCodegen();

        Assert.assertEquals(codegen.toModelName(name), expectedName);
        Assert.assertEquals(codegen.toModelFilename(name), expectedName);
    }

    @Test(dataProvider = "typeNames", description = "correct api name conversion")
    public void apiNameTest(String name, String expectedName) {
        final DefaultCodegen codegen = new FakeCppCodegen();
        final String apiSuffix = "Api";

        Assert.assertEquals(codegen.toApiName(name), expectedName + apiSuffix);
        Assert.assertEquals(codegen.toApiFilename(name), expectedName + apiSuffix);
    }
}
