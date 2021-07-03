package io.swagger.codegen.python;

import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.PythonClientCodegen;

public class PythonClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testProjectNameCharacters() {
        String projectName = ";import os; -;os.system('ping localhost');x=b;_yy=";
        Assert.assertEquals(projectName.replaceAll("[^a-zA-Z0-9\\s\\-_]",""), "import os -ossystemping localhostxb_yy");
        Assert.assertEquals("petstore-api".replaceAll("[^a-zA-Z0-9\\s\\-_]",""), "petstore-api");
        Assert.assertEquals("petstore_api2".replaceAll("[^a-zA-Z0-9\\s\\-_]",""), "petstore_api2");
        Assert.assertEquals("petstore api".replaceAll("[^a-zA-Z0-9\\s\\-_]",""), "petstore api");
    }

}
