package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenType;

public class FakeJavaCodegen extends AbstractJavaCodegen {

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
