package io.swagger.codegen.swift5;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.Swift5Codegen;
import io.swagger.codegen.options.Swift5OptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class Swift5OptionsTest extends AbstractOptionsTest {

    @Tested
    private Swift5Codegen clientCodegen;

    public Swift5OptionsTest() {
        super(new Swift5OptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(Swift5OptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setProjectName(Swift5OptionsProvider.PROJECT_NAME_VALUE);
            times = 1;
            clientCodegen.setResponseAs(Swift5OptionsProvider.RESPONSE_AS_VALUE.split(","));
            times = 1;
            clientCodegen.setUnwrapRequired(Boolean.valueOf(Swift5OptionsProvider.UNWRAP_REQUIRED_VALUE));
            times = 1;
            clientCodegen.setObjcCompatible(Boolean.valueOf(Swift5OptionsProvider.OBJC_COMPATIBLE_VALUE));
            times = 1;
            clientCodegen.setLenientTypeCast(Boolean.valueOf(Swift5OptionsProvider.LENIENT_TYPE_CAST_VALUE));
            times = 1;
        }};
    }
}
