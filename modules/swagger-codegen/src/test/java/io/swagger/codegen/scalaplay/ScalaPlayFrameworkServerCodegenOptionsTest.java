package io.swagger.codegen.scalaplay;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.ScalaPlayFrameworkServerCodegen;
import io.swagger.codegen.options.ScalaPlayFrameworkServerCodegenOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class ScalaPlayFrameworkServerCodegenOptionsTest extends AbstractOptionsTest {

    @Tested
    private ScalaPlayFrameworkServerCodegen codegen;

    public ScalaPlayFrameworkServerCodegenOptionsTest() {
        super(new ScalaPlayFrameworkServerCodegenOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return codegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        // TODO: Complete options
        new Expectations(codegen) {{

        }};
    }
}

