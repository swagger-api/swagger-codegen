package io.swagger.codegen.javascript;

import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.config.CodegenConfigurator;
import org.junit.rules.TemporaryFolder;
import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.JavascriptClientCodegen;

import java.io.File;
import java.io.IOException;

public class JavascriptClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final JavascriptClientCodegen codegen = new JavascriptClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), null);
        Assert.assertEquals(codegen.apiPackage(), "api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), null);
        Assert.assertEquals(codegen.getInvokerPackage(), null);
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), null);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final JavascriptClientCodegen codegen = new JavascriptClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final JavascriptClientCodegen codegen = new JavascriptClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testRecursiveModel() throws IOException {
        final TemporaryFolder folder = new TemporaryFolder();

        folder.create();
        final File output = folder.getRoot();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setLang("javascript")
                .setInputSpec("src/test/resources/2_0/recursive_model.yaml")
                .setOutputDir(output.getAbsolutePath());

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        new DefaultGenerator().opts(clientOptInput).generate();

        final File orderFile = new File(output, "src/api/TestClassApi.js");
        Assert.assertTrue(orderFile.exists());

        folder.delete();

    }

}
