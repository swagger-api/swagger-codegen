package io.swagger.codegen.go;

import io.swagger.codegen.DefaultCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.GoClientCodegen;

public class GoClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test(description = "test enum variable names for reserved words")
    public void testEnumReservedWord() throws Exception {
        final DefaultCodegen codegen = new GoClientCodegen();
        Assert.assertEquals(codegen.toEnumVarName("IF", null), "IF_");
        Assert.assertEquals(codegen.toEnumVarName("const", null), "CONST_");
        Assert.assertEquals(codegen.toEnumVarName("INTERFACE", null), "INTERFACE_");
        // should not escape non-reserved
        Assert.assertEquals(codegen.toEnumVarName("hello", null), "HELLO");
    }

    @Test(description = "test enum variable names for numbers")
    public void testEnumNumber() throws Exception {
        final DefaultCodegen codegen = new GoClientCodegen();
        Assert.assertEquals(codegen.toEnumVarName("1", "int32"), "NUMBER_1");
        Assert.assertEquals(codegen.toEnumVarName("-1", "int32"), "NUMBER_MINUS_1");
        Assert.assertEquals(codegen.toEnumVarName("+1", "int32"), "NUMBER_PLUS_1");
        Assert.assertEquals(codegen.toEnumVarName("1.1", "float64"), "NUMBER_1_DOT_1");
        Assert.assertEquals(codegen.toEnumVarName("-1.2", "float64"), "NUMBER_MINUS_1_DOT_2");
        Assert.assertEquals(codegen.toEnumVarName("+1.2", "float64"), "NUMBER_PLUS_1_DOT_2");
    }

    @Test(description = "test enum variable names for strings that start with a number")
    public void testEnumStringNumber() throws Exception {
        final DefaultCodegen codegen = new GoClientCodegen();
        Assert.assertEquals(codegen.toEnumVarName("1", null), "_1");
        Assert.assertEquals(codegen.toEnumVarName("1_SAMPLE", null), "_1_SAMPLE");
    }

}
