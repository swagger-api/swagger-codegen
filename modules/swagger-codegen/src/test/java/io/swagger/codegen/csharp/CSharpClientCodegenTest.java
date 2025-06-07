package io.swagger.codegen.csharp;

import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.CSharpClientCodegen;

import java.io.File;

public class CSharpClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test(description = "TEST_FOLDER should be updated with SOURCE_FOLDER when SOURCE_FOLDER is set")
    public void testAdditionalPropertiesPutForSourceFolder() throws Exception {
        final CSharpClientCodegen codegen = new CSharpClientCodegen();

        final String expectedSourceFolder = "CustomSourceFolder";

        codegen.additionalProperties().put(CodegenConstants.SOURCE_FOLDER, expectedSourceFolder);
        codegen.processOpts();

        final String expectedTestFolder = codegen.outputFolder() + File.separator + expectedSourceFolder;

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.SOURCE_FOLDER), expectedSourceFolder);
        Assert.assertTrue(codegen.apiTestFileFolder().startsWith(expectedTestFolder));
        Assert.assertTrue(codegen.modelTestFileFolder().startsWith(expectedTestFolder));
    }

}
