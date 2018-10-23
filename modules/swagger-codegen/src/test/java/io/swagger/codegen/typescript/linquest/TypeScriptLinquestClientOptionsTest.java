package io.swagger.codegen.typescript.linquest;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.TypeScriptLinquestClientCodegen;
import io.swagger.codegen.options.TypeScriptLinquestClientOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class TypeScriptLinquestClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private TypeScriptLinquestClientCodegen clientCodegen;

    public TypeScriptLinquestClientOptionsTest() {
        super(new TypeScriptLinquestClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptLinquestClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setModelPropertyNaming(TypeScriptLinquestClientOptionsProvider.MODEL_PROPERTY_NAMING_VALUE);
            times = 1;
            clientCodegen.setSupportsES6(TypeScriptLinquestClientOptionsProvider.SUPPORTS_ES6_VALUE);
            times = 1;
        }};
    }
}
