package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;

public class FakeScalaCodeGen extends AbstractScalaCodegen implements CodegenConfig{
    public FakeScalaCodeGen() {
        super();
        this.reservedWords.add("reservedword");
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "io.swagger.codegen.languages.FakeScalaCodeGen";
    }

    @Override
    public String getHelp() {
        return "help";
    }
}
